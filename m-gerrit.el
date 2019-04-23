
(require 'magit)

(defvar-local p-magit-gerrit-ssh-creds nil
  "Credentials used to execute gerrit commands via ssh of the form ID@Server")

(defvar-local p-magit-gerrit-remote "origin"
  "Default remote name to use for gerrit (e.g. \"origin\", \"gerrit\")")

(defun p-gerrit-command (cmd &rest args)
  (let ((gcmd (concat
	             "-x -p 29418 "
	             (or p-magit-gerrit-ssh-creds
		               (error "`p-magit-gerrit-ssh-creds' must be set!"))
	             " "
	             "gerrit "
	             cmd
	             " "
	             (mapconcat 'identity args " "))))
    (message (format "Using cmd: %s" gcmd))
    gcmd))

(defun p-gerrit-command (cmd &rest args)
  (let ((gcmd (concat
	             "-x -p 29418 "
	             (or p-magit-gerrit-ssh-creds
		               (error "`p-magit-gerrit-ssh-creds' must be set!"))
	             " "
	             "gerrit "
	             cmd
	             " "
	             (mapconcat 'identity args " "))))
    (message (format "Using cmd: %s" gcmd))
    gcmd))


(defun p-magit-gerrit-get-remote-url ()
  (magit-git-string "ls-remote" "--get-url" p-magit-gerrit-remote))

(defun p-magit-gerrit-get-project ()
  (let* ((regx (rx (zero-or-one ?:) (zero-or-more (any digit)) ?/
		               (group (not (any "/")))
		               (group (one-or-more (not (any "."))))))
	       (str (or (p-magit-gerrit-get-remote-url) ""))
	       (sstr (car (last (split-string str "//")))))
    (when (string-match regx sstr)
      (concat (match-string 1 sstr)
	            (match-string 2 sstr)))))

(defun p-gerrit-query (prj &optional status)
  (message (format "query using project: %s" prj))
  (p-gerrit-command "query"
		              "--format=JSON"
		              "--all-approvals"
		              "--comments"
		              "--current-patch-set"
		              (concat "project:" prj)
		              (concat "status:" (or status "open"))))


(defun p-magit-gerrit-wash-review ()
  (message "washing review")
  (let* ((beg (point))
	 (jobj (json-read))
	 (end (point))
	 (num (cdr-safe (assoc 'number jobj)))
	 (subj (cdr-safe (assoc 'subject jobj)))
	 (owner (cdr-safe (assoc 'owner jobj)))
	 (owner-name (cdr-safe (assoc 'name owner)))
	 (owner-email (cdr-safe (assoc 'email owner)))
	 (patchsets (cdr-safe (assoc 'currentPatchSet jobj)))
	 ;; compare w/t since when false the value is => :json-false
	 (isdraft (eq (cdr-safe (assoc 'isDraft patchsets)) t))
	 (approvs (cdr-safe (if (listp patchsets)
				(assoc 'approvals patchsets)
			      (assoc 'approvals (aref patchsets 0))))))

    (if (and beg end)
	(delete-region beg end))
    (when (and num subj owner-name)
      (magit-insert-section (section subj)
	(insert (propertize
		 (p-magit-gerrit-pretty-print-review num subj owner-name isdraft)
		 'p-magit-gerrit-jobj
		 jobj))
	(unless (oref (magit-current-section) hidden)
	  (p-magit-gerrit-wash-approvals approvs))
	(add-text-properties beg (point) (list 'p-magit-gerrit-jobj jobj)))
      t)))

(defun p-magit-gerrit-pretty-print-review (num subj owner-name &optional draft)
  ;; window-width - two prevents long line arrow from being shown
  (let* ((wid (- (window-width) 2))
	       (numstr (propertize (format "%-10s" num) 'face 'magit-hash))
	       (nlen (length numstr))
	       (authmaxlen (/ wid 4))

	       (author (propertize (p-magit-gerrit-string-trunc owner-name authmaxlen)
			                       'face 'magit-log-author))

	       (subjmaxlen (- wid (length author) nlen 6))

	       (subjstr (propertize (p-magit-gerrit-string-trunc subj subjmaxlen)
			                        'face
			                        (if draft
				                          'magit-signature-bad
				                        'magit-signature-good)))
	       (authsubjpadding (make-string
			                     (max 0 (- wid (+ nlen 1 (length author) (length subjstr))))
			                     ? )))
    (format "%s%s%s%s\n"
	          numstr subjstr authsubjpadding author)))

(defun p-magit-gerrit-string-trunc (str maxlen)
  (if (> (length str) maxlen)
      (concat (substring str 0 maxlen)
	            "...")
    str))

(defun p-magit-gerrit-wash-approvals (approvals)
  (mapc #'p-magit-gerrit-wash-approval approvals))

(defun p-magit-gerrit-wash-approval (approval)
  (let* ((approver (cdr-safe (assoc 'by approval)))
	       (approvname (cdr-safe (assoc 'name approver)))
	       (approvemail (cdr-safe (assoc 'email approver)))
	       (type (cdr-safe (assoc 'type approval)))
	       (verified (string= type "Verified"))
	       (codereview (string= type "Code-Review"))
	       (score (cdr-safe (assoc 'value approval))))

    (magit-insert-section (section approval)
      (insert (p-magit-gerrit-pretty-print-reviewer approvname approvemail
						                                       (and codereview score)
						                                       (and verified score))
	            "\n"))))

(defun p-magit-gerrit-pretty-print-reviewer (name email crdone vrdone)
  (let* ((wid (1- (window-width)))
	       (crstr (propertize (if crdone (format "%+2d" (string-to-number crdone)) "  ")
			                      'face '(magit-diff-lines-heading
				                            bold)))
	       (vrstr (propertize (if vrdone (format "%+2d" (string-to-number vrdone)) "  ")
			                      'face '(magit-diff-added-highlight
				                            bold)))
	       (namestr (propertize (or name "") 'face 'magit-refname))
	       (emailstr (propertize (if email (concat "(" email ")") "")
			                         'face 'change-log-name)))
    (format "%-12s%s %s" (concat crstr " " vrstr) namestr emailstr)))

(defun p-magit-gerrit-section (section title washer &rest args)
  (let ((magit-git-executable "ssh")
	      (magit-git-global-arguments nil))
    (magit-insert-section (section title)
      (message (format "insert head %s, args: %S" title args))
      (magit-insert-heading title)
      (magit-git-wash washer (split-string (car args)))
      (insert "\n"))))

(defun p-magit-gerrit-wash-reviews (&rest args)
  (magit-wash-sequence #'p-magit-gerrit-wash-review))

(defun p-magit-insert-gerrit-reviews ()
  (p-magit-gerrit-section 'p-gerrit-reviews
			                   "PReviews:" 'p-magit-gerrit-wash-reviews
			                   (p-gerrit-query (p-magit-gerrit-get-project))))

(defcustom p-magit-gerrit-popup-prefix (kbd "R")
  "Key code to open p-magit-gerrit popup"
  :group 'p-magit-gerrit
  :type 'key-sequence)

(defvar p-magit-gerrit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map p-magit-gerrit-popup-prefix 'p-magit-gerrit-popup)
    map))

(define-minor-mode p-magit-gerrit-mode "Gerrit support for Magit - pangwa"
  :lighter " GerritP" :require 'magit-topgit
  (or (derived-mode-p 'magit-mode)
      (error "This mode only makes sense with magit"))
  (or p-magit-gerrit-ssh-creds
      (error "You *must* set `p-magit-gerrit-ssh-creds' to enable p-magit-gerrit-mode"))
  (or (p-magit-gerrit-get-remote-url)
      (error "You *must* set `p-magit-gerrit-remote' to a valid Gerrit remote"))
  (cond
   (p-magit-gerrit-mode
    (magit-add-section-hook 'magit-status-sections-hook
			    'p-magit-insert-gerrit-reviews
			    t)
    )

   (t
    (remove-hook 'magit-after-insert-stashes-hook
		 'p-magit-insert-gerrit-reviews t)
    ))
  (when (called-interactively-p 'any)
    (magit-refresh)))

(define-transient-command p-magit-gerrit-review ()
  "Gerrit review commands"
  [["Send"
    ("r" "send review"          p-magit-gerrit-create-review)
    ("w" "send review as wip"    p-magit-gerrit-create-wip-review)]
   ["Review"
    ("a" "Review +2" p-magit-gerrit-score-2)
    ("b" "Review +1" p-magit-gerrit-score-1)
    ("c" "Review -1" p-magit-gerrit-score-minus-1)
    ("d" "Review -2" p-magit-gerrit-score-minus-2)
    ("p" "Show patchset" p-magit-gerrit-view-patchset-diff)
    ]])

(defun p-magit-gerrit-score-2 ()
  (interactive)
  (p-magit-gerrit-code-review "2"))

(defun p-magit-gerrit-score-1 ()
  (interactive)
  (p-magit-gerrit-code-review "1"))

(defun p-magit-gerrit-score-minus-1 ()
  (interactive)
  (p-magit-gerrit-code-review "-1"))

(defun p-magit-gerrit-score-minus-2 ()
  (interactive)
  (p-magit-gerrit-code-review "-2"))

(defun p-magit-gerrit-code-review (score)
  "Perform a Gerrit Code Review"
  (let ((rev (cdr-safe (assoc
		                    'revision
		                    (cdr-safe (assoc 'currentPatchSet
				                                 (p-magit-gerrit-review-at-point))))))
	      (prj (p-magit-gerrit-get-project)))
    (p-gerrit-code-review prj rev score)
    (magit-refresh)))

(defun p-gerrit-code-review (prj rev score &optional msg)
  (gerrit-ssh-cmd "review" "--project" prj "--code-review" score
		              (if msg msg "") rev))

(defun p-magit-gerrit-review-at-point ()
  (get-text-property (point) 'p-magit-gerrit-jobj))


(defun p-magit-gerrit-create-review ()
  (interactive)
  (p-magit-gerrit-push-review 'publish))

(defun p-magit-gerrit-create-wip-review()
  (interactive)
  (p-magit-gerrit-push-review 'wip))

(defun p-magit-gerrit-push-review (status)
  (let* ((branch (or (magit-get-current-branch)
		     (error "Don't push a detached head.  That's gross")))
	 (commitid (or (when (eq (oref (magit-current-section) type)
				 'commit)
			 (oref (magit-current-section) value))
		       (error "Couldn't find a commit at point")))
	 (rev (magit-rev-parse (or commitid
				   (error "Select a commit for review"))))

	 (branch-remote (and branch (magit-get "branch" branch "remote"))))

    ;; (message "Args: %s "
    ;;	     (concat rev ":" branch-pub))

    (let* ((branch-merge (if (or (null branch-remote)
				 (string= branch-remote "."))
			     (completing-read
			      "Remote Branch: "
			      (let ((rbs (magit-list-remote-branch-names)))
				(mapcar
				 #'(lambda (rb)
				     (and (string-match (rx bos
							    (one-or-more (not (any "/")))
							    "/"
							    (group (one-or-more any))
							    eos)
							rb)
					  (concat "refs/heads/" (match-string 1 rb))))
				 rbs)))
			   (and branch (magit-get "branch" branch "merge"))))
	   (branch-pub (progn
			 (string-match (rx "refs/heads" (group (one-or-more any)))
				       branch-merge)
			 (format "refs/%s%s/%s" status (match-string 1 branch-merge) branch))))


      (when (or (null branch-remote)
		(string= branch-remote "."))
	(setq branch-remote p-magit-gerrit-remote))

      (magit-run-git-async "push" "-v" branch-remote
			   (concat rev ":" branch-pub)))))

(defun p-magit-gerrit-view-patchset-diff ()
  "View the Diff for a Patchset"
  (interactive)
  (let ((jobj (p-magit-gerrit-review-at-point)))
    (when jobj
      (let ((ref (cdr (assoc 'ref (assoc 'currentPatchSet jobj))))
            (dir default-directory))
        (let* ((magit-proc (magit-git-fetch p-magit-gerrit-remote ref)))
          (message (format "Waiting a git fetch from %s to complete..."
                           p-magit-gerrit-remote))
          (p-magit-gerrit-process-wait))
        (message (format "Generating Gerrit Patchset for refs %s dir %s" ref dir))
        (magit-show-commit "FETCH_HEAD")))))

(defsubst p-magit-gerrit-process-wait ()
  (while (and magit-this-process
	            (eq (process-status magit-this-process) 'run))
    (sleep-for 0.005)))

(transient-append-suffix 'magit-dispatch "z"
  '("C" "Code Review" p-magit-gerrit-review))

(defun p-magit-gerrit-check-enable ()
  (let ((remote-url (p-magit-gerrit-get-remote-url)))
    (when (and remote-url
	             (or p-magit-gerrit-ssh-creds
		               (p-magit-gerrit-detect-ssh-creds remote-url)))
      (p-magit-gerrit-mode t))))

(add-hook 'magit-status-mode-hook #'p-magit-gerrit-check-enable t)

(provide 'm-gerrit)
