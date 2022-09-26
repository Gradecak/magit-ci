(require 'magit-ci)


(defun magit-ci--gh-actions-command-builder (git-branch)
  (let* ((json-fields "--json=createdAt,headBranch,headSha,name,url,updatedAt,status,conclusion")
	 (filter-query (format "--jq=[.[] | select(.headBranch == %S)]" git-branch))
	 (command (append '("gh" "run" "list") `(,json-fields ,filter-query))))
    command))


(defun magit-ci--gh-actions-get-status (run)
  ;; TODO: the status processing is a little bit weird since I have no
  ;; familiarity with github actions. It should be revisited
  (pcase (gethash "status" run)
    ("completed" (if (equal
		      (gethash "conclusion" run)
		      '("failure" "timed_out" "startup_failure" "action_required"))
		     'failed
		   'success))
    ("in_progress" 'in-progress)
    (_ 'unknown)))

(defun magit-ci--gh-actions-parse-build (run)
  (ci-build
   :branch (gethash "headBranch" run)
   :status (magit-ci--gh-actions-get-status run)
   :log-url (gethash "url" run)
   ;; TODO: the individual step don't seem to be easily accessible through gh
   ;; However, this might be possible to grab through the `gh api` process?
   :steps '()
   :commit-ref (substring (gethash "headSha" run) 0 7)
   :start-time (magit-ci--iso-parse-time (gethash "createdAt" run))
   :end-time (magit-ci--iso-parse-time (gethash "updatedAt" run))))

(magit-ci-defsource
 "gh-actions"
 :command-builder #'magit-ci--gh-actions-command-builder
 :response-parser #'(lambda (string) (mapcar
				      #'magit-ci--gh-actions-parse-build
				      (json-parse-string string))))
