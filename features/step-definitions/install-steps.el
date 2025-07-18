;; -*- lexical-binding: t; -*-
(defvar basic-test-elpaca-loaded nil)
(Given "^dot emacs exists$"
  (lambda ()
    (should (file-exists-p ".emacs"))))

(When "^it loads from scratch$"
  (lambda ()
    (add-hook 'elpaca-after-init-hook
              (lambda () (setq basic-test-elpaca-loaded t)))
    (setq elpaca-verbosity 100)
    (load ".emacs")))

(Then "^it should install Elpaca$"
  (lambda ()
    (should (file-exists-p
             (expand-file-name "early-init.el" user-emacs-directory)))
    (should (require 'elpaca))))

(And "^it should install packages$"
  (lambda ()
    (let ((deps (mapcar (lambda (item)
                          (let ((dep-name (car item))
                                (repo (plist-get
                                       (plist-get (cdr item) :recipe) :repo)))
                            (cond ((eq (type-of repo) 'string)
                                   `(,dep-name . ,(file-name-base repo)))
                                  ((eq (type-of repo) 'cons)
                                   `(,dep-name . ,(file-name-base (cdr repo))))
                                  (t (error "unhandled: %s" (type-of repo))))))
                        (with-temp-buffer
                          (insert-file-literally ".emacs.elpaca.lock")
                          (read (buffer-string))))))
      (let ((pass-max-tries 3)
            (package-max-tries 3)
            (repos (expand-file-name
                    (file-name-concat user-emacs-directory "elpaca" "repos")))
            (fail-count 0)
            (fail-idx 0)
            (pass-tries 0)
            (package-tries 0)
            (initial-successes 0))
        ;; initial wait
        (elpaca-wait)
        (setq fail-count (alist-get 'failed elpaca--status-counts))
        (setq initial-successes
              (or (alist-get 'finished elpaca--status-counts) 0))

        ;; check and retry
        (catch 'no-failures
          (dotimes (pass-tries pass-max-tries)
            (setq package-tries 0)

            (unless fail-count
              (throw 'no-failures nil))

            (message "--- Checking failures (%s/%s) ---"
                     (1+ pass-tries) pass-max-tries)

            ;; note: re-building may cause conflicts in the internal dep tree
            ;;       when hitting everything around with "rebuild"
            ;;       so rebuild one, then re-check, order is not important much
            (let (pkg)
              (catch 'rebuild-done
                (dotimes (package-tries package-max-tries)
                  (unless pkg
                    (catch 'find-done
                      (dolist (dep deps)
                        (when (eq (car (elpaca<-statuses
                                        (elpaca-get (car dep))))
                                  'failed)
                          (setq pkg (car dep))
                          (throw 'find-done nil)))))
                  (unless (eq (car (elpaca<-statuses (elpaca-get pkg)))
                              'failed)
                    (message "--: Rebuilding: %S (%s/%s) | (%s/%s) done :--"
                             pkg (1+ package-tries) package-max-tries
                             (1+ pass-tries) pass-max-tries)
                    (throw 'rebuild-done nil))

                  (message "--! Rebuilding: %S (%s/%s) | (%s/%s) !--"
                           pkg (1+ package-tries) package-max-tries
                           (1+ pass-tries) pass-max-tries)
                  (elpaca-rebuild pkg)
                  (elpaca-wait)
                  (setq initial-successes
                        (+ initial-successes
                           (or (alist-get 'finished elpaca--status-counts) 0)))

                  (setq fail-count
                        (or (alist-get 'failed elpaca--status-counts) 0))
                  (when (= 0 fail-count)
                    ;; note: hit the head and resolved failures - its deps
                    (message "--- Nothing left for rebuilding ---")
                    (throw 'no-failures nil)))))))
        (dolist (dep deps)
          (unless (file-exists-p (file-name-concat repos (cdr dep)))
            (message "Missing dependency: %S | %S | %S | %S"
                     (car dep) (elpaca<-statuses (elpaca-get (car dep)))
                     basic-test-elpaca-loaded
                     (file-name-concat repos (cdr dep))))))

      ;; note: check .emacs, compare with lock file behavior
      (let ((pattern (rx line-start (*? whitespace)
                         (literal "(use-package") (*? whitespace)
                         (group (+ (or lower upper "-"))))))
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert-file-literally ".emacs")
          (goto-char (point-min))

          (unwind-protect
              (when (re-search-forward pattern nil t)
                ;; note: go through each use-package and try importing
                ;; note: (require) failing == package installation failed
                (require (intern (buffer-substring-no-properties
                                  (match-beginning 1) (match-end 1)))))
            ;; todo: why isn't this auto-set by Elpaca in the hook?
            (message "basic-test-elpaca-loaded: %s" basic-test-elpaca-loaded)
            (setq basic-test-elpaca-loaded t))))

      (dolist (queue elpaca--queues)
        (when (and (not (eq (elpaca-q<-status queue) 'complete))
                   ;; note(elpaca): processed==0 might mean it's stuck hmm
                   (> (elpaca-q<-processed queue) 0)
                   ;; nitpick(elpaca): empty noise queue? why is it even there?
                   (or (elpaca-q<-autoloads queue)
                       (elpaca-q<-forms queue)
                       (elpaca-q<-elpacas queue)))
          (error "Queue failed: %s" queue)))

      (unless (and (= (length deps)
                      (or (alist-get 'finished elpaca--status-counts) 0))
                   (not basic-test-elpaca-loaded))
        (with-current-buffer (get-buffer-create elpaca-log-buffer)
          (message "--- elpaca log beg buffer ---")
          (message "%s" (buffer-string))
          (message "--- elpaca log end buffer ---")
          (message "--- elpaca beg UI entries (in log) ---")
          (when (boundp 'elpaca-ui-entries)
            (message "raw: >%S<" elpaca-ui-entries)

            (dolist (pkg elpaca-ui-entries)
              (message "*** --- elpaca beg package %S ---" pkg)
              (message "%s" (ignore-errors (elpaca-info (caar pkg))))
              (message "*** --- elpaca end package %S ---" pkg)))
          (message "--- elpaca end UI entries (in log) ---")
          (message "--- elpaca beg UI tab entries (in log) ---")
          (when (boundp 'tabulated-list-entries)
            (message "%S" tabulated-list-entries))
          (message "--- elpaca end UI tab entries (in log) ---"))
        (with-current-buffer (get-buffer-create "*Warnings*")
          (message "--- warnings beg buffer ---")
          (message "%s" (buffer-string))
          (message "--- warnings end buffer ---"))
        (with-current-buffer (get-buffer-create "*Compile-Log*")
          (message "--- compile beg buffer ---")
          (message "%s" (buffer-string))
          (message "--- compile end buffer ---"))
        (should (= (length deps)
                   (or (alist-get 'finished elpaca--status-counts) 0)))))))
