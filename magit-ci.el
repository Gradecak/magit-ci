(require 'magit)
(require 'cl-lib)
(require 'parse-time)

;; Inline Jira
;; DONE use async when fetching the build status so that we don't block magit
;; DONE only display builds for current branch
;; DONE dynamically configure 'extra branches' to watch. right now hardcoded to 'preview'
;; DONE cache results so that we can display them while we update in the background
;; DONE parse the individual steps so that we can see the status in more detail
;; DONE render individual steps as a subsection to the build step
;; DONE figure out how to format the output when we have long branch names so that everything aligns
;; DONE parse the start and end time to calculate time elapsed
;; DONE save the currently running gcloud process to prevent multiple concurrent gcloud polls
;; DONE if any state is currently in progress, automatically keep polling until everything is in a terminal state
;; DONE improve formatting of the CI section in magit buffer
;; DONE render runtime in human readable format (minutes + seconds)
;; TODO replace the magit-ci--delete-section function with some sort of an abstraction over (magit-insert-section) macro that first deletes before inserting
;; DONE filter out response from gcloud so that it removes duplicates and preserves latest
;; DONE cache output for multiple branches
;; DONE show loading indicator if cache is empty for the branch we switch to
;; DONE capture the URL of the cloudbuild logs so that the enter keypress takes you to the page
;; TODO figure out why we can't go to next section when the builds are inserted from cache
;; DONE on re-render respect the previous state of folded/unfolded for section
;; DONE render a 'status' indicator to display to the user when there is a fetch happening
;; TODO improve how we render the loading indicator
;; TODO save point when the magit buffer is refreshed so that the cursor doesn't jump back to the top when the async load is finished

(defvar magit-ci-gcloud-project "bloomon-build")

(defvar-local magit-ci--builds-cache '())
(defvar-local magit-ci--fetch-process nil
  "Note down if we are currently waiting for the results of a CI fetch.
Prevents the running of multiple update processes concurrently")

(defvar magit-ci-source #'magit-ci-gcloud-source
  "What to use to fetch ci builds.")

(defvar magit-ci-extra-branches '("preview" "master")
  "A sequence of extra branches to watch other than the branch.
By default magit-ci only watches your currently checked out branch.")

(defface magit-ci-success
    '((((class color) (background light)) :foreground "LawnGreen")
      (((class color) (background  dark)) :foreground "PaleGreen"))
  "Face for sucessful CI build status."
  :group 'magit-ci-faces)

(defface magit-ci-pending
    '((((class color) (background light)) :foreground "orange2")
      (((class color) (background  dark)) :foreground "orange2"))
  "Face for pending CI build status."
  :group 'magit-ci-faces)

(defface magit-ci-fail
    '((((class color) (background light)) :foreground "coral3")
      (((class color) (background  dark)) :foreground "coral3"))
  "Face for fail CI build status."
  :group 'magit-ci-faces)

(defface magit-ci-unknown
    '((((class color) (background light)) :foreground "gray")
      (((class color) (background  dark)) :foreground "gray"))
  "Face for unknown CI build status."
  :group 'magit-ci-faces)

(defun magit-ci--status-string (status)
  (propertize
   (symbol-name status)
   'face
   (pcase status
     ('success 'magit-ci-success)
     ('in-progress 'magit-ci-pending)
     ('failed 'magit-ci-fail)
     ('unknown 'magit-ci-unknown))))

(defclass ci-base ()
  ((start-time :initarg :start-time)
   (end-time :initarg :end-time)
   (status :initarg :status)))

(defclass ci-build (ci-base)
  ((branch :initarg :branch)
   (steps :initarg :steps)
   (log-url :initarg :log-url)
   (commit-ref :initarg :commit-ref))
  "A datatype capturing a CI run")

(cl-defmethod ci-build-runtime ((obj ci-base))
  "Get the runtime of the ci-build OBJ using :end-time and :start-time."
  (with-slots (end-time start-time) obj
    (time-subtract (or end-time (current-time)) start-time)))

(defvar magit-build-item-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-visit-thing] #' magit-ci--visit-build)
    map)
  "Keymap for build section.")

(cl-defun magit-ci--visit-build (&key (build-url (oref (magit-current-section) value)))
  (interactive)
  (browse-url build-url))

(cl-defmethod ci-build-render-magit-section ((obj ci-build))
  "Insert a magit section into the 'current-buffer' for the ci-build OBJ."
  (with-slots (branch status steps commit-ref) obj
    (let* ((build-url (oref obj log-url))
	   (build-status-section
	    (magit-insert-section  (build-item build-url)
		(insert (format "%s\t%s\t%s\t%s\n"
				(propertize commit-ref 'face 'magit-hash)
				branch
				(magit-ci--status-string status)
				(format-time-string "%M:%S" (ci-build-runtime obj))))
	      (magit-insert-heading)
	      (mapcar #'ci-step-render-magit-section steps))))
      (pcase (magit-section-cached-visibility build-status-section)
	('show (magit-section-show build-status-section))
	(_ (magit-section-hide build-status-section))))))

(defun magit-ci--iso-parse-time (time-str)
  "Provide a sane way to parse a TIME-STR into a useable timestamp.
This timestamp can actually be used by other Emacs time functions."
  (encode-time (decoded-time-set-defaults (parse-time-string time-str))))

(defclass ci-step (ci-base)
  ((action :initarg :action))
  "Model the individual step of a CI run")

(cl-defmethod ci-step-render-magit-section ((obj ci-step))
  (let ((curr-pt (point)))
    (with-slots (status start-time action) obj
      (magit-insert-section (build-status )
	  (insert (format " └%s " (magit-ci--status-string status)))
	;; sometimes start time of steps is not known. If there is no start time we can't
	;; provide an accurate runtime
	(when (not (null start-time))
          (insert (format "%s " (format-time-string "%M:%S" (ci-build-runtime obj)))))
	(insert (format "%s \n" action))))))

(defun magit-ci--gcloud-parse-step (step-data)
  (let ((step (ci-step
               :start-time nil
               :end-time nil
	       :status (magit-ci--gcloud-parse-status (gethash "status" step-data))
	       :action (string-join
                        (append
                         `(,(gethash "name" step-data))
                         (append (gethash "args" step-data) nil )) " "))))
    ;; timing information doesn't exist for queued steps
    (when-let (timing (gethash "timing" step-data))
      (when-let ((start-time (gethash "startTime" timing)))
        (oset step :start-time (magit-ci--iso-parse-time start-time)))
      (when-let (end-time (gethash "endTime" timing))
        (oset step :end-time (magit-ci--iso-parse-time end-time))))
    step))

(defun magit-ci--gcloud-parse-status (status)
  "Translate a gcloud ci STATUS string into a magit-ci recognised status."
  (pcase status
    ("QUEUED" 'queued)
    ("WORKING" 'in-progress)
    ("SUCCESS" 'success)
    ("FAILURE" 'failed)
    (_ 'unknown)))

(defun magit-ci--gcloud-parse-build (item)
  (let* ((substitutions (gethash "substitutions" item))
	 (build (ci-build
                 :branch (gethash "BRANCH_NAME" substitutions)
                 :status (magit-ci--gcloud-parse-status (gethash "status" item))
                 :steps (mapcar #'magit-ci--gcloud-parse-step (gethash "steps" item))
		 :log-url (gethash "logUrl" item)
		 :commit-ref (gethash "SHORT_SHA" substitutions)
                 :start-time nil
                 :end-time nil)))
    (when-let ((start-time (gethash "startTime" item)))
      (oset build :start-time (magit-ci--iso-parse-time start-time)))
    (when-let ((end-time (gethash "finishTime" item)))
      (oset build :end-time (magit-ci--iso-parse-time end-time)))
    build))

(defun magit-ci--render-section (builds)
  "Insert a list of ci-builds BUILDS into as a magit section."
  (save-excursion
    (let (;; HACK taken from magit-todos. Allows the newly inserted section to be
	  ;; collapsable like the other section
	  (inhibit-read-only t)
	  (magit-insert-section--parent magit-root-section))
      (magit-ci--delete-section [* magit-ci-section])
      (goto-char (point-max))
      (magit-insert-section (magit-ci-section)
	  (magit-insert-heading (if (process-live-p magit-ci--fetch-process)
				    "CI Builds (*)"
				  "CI Builds"))
	(cond
	  ((not (null builds))
	   (let ((current-point (point-max))
		 (align-default-spacing 2))
	     (insert (propertize "ref\tbranch\tstatus\truntime\n" 'face 'font-lock-warning-face))
	     (mapcar #'ci-build-render-magit-section builds)
	     (align-regexp current-point (point-max) "\\(\\s-*\\)\t" 1 1 t)))
	  ((and (null builds) (not (null magit-ci--fetch-process)))
	   (insert "Loading...\n"))
	  (t (insert "No builds \n")))))))

(defun magit-ci--delete-section (condition)
  "Delete the section specified by CONDITION from the Magit status buffer.
See `magit-section-match'.  Also delete it from root section's children."
  (save-excursion
    (goto-char (point-min))
    (when-let ((section (cl-loop until (magit-section-match condition)
                              ;; Use `forward-line' instead of `magit-section-forward' because
                              ;; sometimes it skips our section.
                              do (forward-line 1)
                              when (eobp)
                              return nil
                              finally return (magit-current-section))))
      ;; Delete the section from root section's children.  This makes the section-jumper command
      ;; work when a replacement section is inserted after deleting this section.
      (object-remove-from-list magit-root-section 'children section)
      (with-slots (start end) section
        ;; NOTE: We delete 1 past the end because we insert a newline after the section.  I'm not
        ;; sure if this would generalize to all Magit sections.  But if the end is the same as
        ;; `point-max', which may be the case if todo items have not yet been inserted, we only
        ;; delete up to `point-max'.
        (delete-region start (if (= end (point-max))
                                 end
                               (1+ end)))))))

(defun magit-ci--gcloud-command-builder (git-branch)
  (let ((base-command '("gcloud" "beta" "builds" "list" "--format=json" "--verbosity=none"))
	(project-arg (concat "--project=" magit-ci-gcloud-project))
	(filters (string-join
		  `(,(concat "substitutions.REPO_NAME~" (projectile-project-name))
		     ,(format "substitutions.BRANCH_NAME=(%s)"
			      (string-join
			       (append `(,git-branch) magit-ci-extra-branches) " OR ")))
		  " AND ")))
    (append base-command `(,project-arg) `("--filter" ,filters))))

(defun magit-ci--process-ci-response (response-parser magit-buffer git-branch process)
  (with-current-buffer (process-buffer process)
    (let* ((builds (funcall response-parser (buffer-substring (point-min) (point-max))))
	   (filtered '())
	   (branches
	    (cl-delete-duplicates
	     (mapcar (lambda (v) (intern (slot-value v :branch))) builds)))
	   (build-in-progress-p
	    (lambda (build) (not (memq (slot-value build :status) '(success failed))))))

      ;; filter out duplicate builds, prefering the latest one.
      ;; XXX potentially extract this into a function so that users can supply their own
      ;; filtering strategy?
      (cl-loop for build in builds
	    do (with-slots (branch start-time) build
		 (if-let ((branch (intern branch))
			  (candidate (plist-get filtered branch)))
		     (when (time-less-p (slot-value candidate :start-time) start-time)
		       (setq filtered (plist-put filtered branch build)))
		   (setq filtered (plist-put filtered branch build)))))
      (setq builds (mapcar (lambda (branch) (plist-get filtered branch)) branches))

      ;; save the items that we're about to render as last result of gcloud build list
      ;; as builds cache is buffer local change the buffer context
      (with-current-buffer magit-buffer
	(setq
	 magit-ci--fetch-process nil
	 magit-ci--builds-cache (push `(,(intern git-branch) . ,builds)
				      magit-ci--builds-cache))
	;; if there is a build in progress, schedule the next fetch from the ci source
	(when (cl-some build-in-progress-p builds)
	  (funcall magit-ci-source))

	(when (buffer-live-p magit-buffer)
	  ;; remove any previous section that was there before the async update was
	  ;; finished
	  ;; perform the insert into the magit buffer
	  (magit-ci--render-section builds))))))

(cl-defmacro magit-ci-defsource (name &key command-builder response-parser)
  "Create a CI source by executing the command created by
COMMAND-BUILDER and parse the output of process using
RESPONSE-PARSER. Parsed results are rendered into the magit buffer"
  `(defun ,(intern (format "magit-ci-%s-source" name)) (&optional magit-buffer)
     (let* ((git-branch (magit-get-current-branch))
	    (command (funcall ,command-builder git-branch))
	    (callback
	     (apply-partially
	      #'magit-ci--process-ci-response
	      ,response-parser (or magit-buffer (current-buffer)) git-branch)))

       ;; XXX possibly change this so that the extra branches still get rendered, rather
       ;; than nothing when a rebase is in progress
       (when (not (null git-branch))
	 ;; if we're not already waiting for the output of an async process. Schedule
	 ;; a fetch, otherwise we wait for output of currently running fetch
	 (when (or (null magit-ci--fetch-process)
		   (not (process-live-p magit-ci--fetch-process)))
	   (setq magit-ci--fetch-process
		 (apply #'async-start-process
			(car command) (car command) callback (cdr command))))

	 ;; render a cached build while we fetch the update. This makes the update seem a
	 ;; bit more seamless
	 (magit-ci--render-section
	  (alist-get (intern git-branch) magit-ci--builds-cache))))))

(magit-ci-defsource
 "gcloud"
 :command-builder #'magit-ci--gcloud-command-builder
 :response-parser #'(lambda (string) (mapcar
				      #'magit-ci--gcloud-parse-build
				      (json-parse-string string))))

;;;###autoload
(define-minor-mode magit-ci-mode
    "Add a 'CI-Builds section to the Magit status buffer."
  :require 'magit-ci
  :group 'magit-ci
  :global t
  (if magit-ci-mode
      (magit-add-section-hook 'magit-status-sections-hook magit-ci-source)
    (remove-hook 'magit-status-sections-hook magit-ci-source)))

(provide 'magit-ci)
