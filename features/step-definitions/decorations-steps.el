;; -*- lexical-binding: t; -*-
(When "^emacs loads$"
  (lambda ()
    (should after-init-time)
    (require 'elpaca)
    (elpaca-wait)
    (dolist (queue elpaca--queues)
      (when (and (not (eq (elpaca-q<-status queue) 'complete))
                 ;; note(elpaca): processed==0 might mean it's stuck hmm
                 (> (elpaca-q<-processed queue) 0)
                 ;; nitpick(elpaca): empty noise queue? why is it even there?
                 (or (elpaca-q<-autoloads queue)
                     (elpaca-q<-forms queue)
                     (elpaca-q<-elpacas queue)))
        (message "Queue failed: %s" queue)))
    (should-not (alist-get 'failed elpaca--status-counts))
    ;; todo(elpaca): why are you broken?
    ;; (should elpaca-after-init-time)
    ))

(Then "^toolbar is hidden$"
  (lambda ()
    (should-not tool-bar-mode)))

(And "^scrollbar is hidden$"
  (lambda ()
    (require 'scroll-bar)
    (should-not scroll-bar-mode)))

(And "^file dialog is hidden$"
  (lambda ()
    (should-not use-file-dialog)))

(And "^fringe is \"\\([^\"]+\\)\" pixels wide$"
  (lambda (arg)
    (should (= (string-to-number arg) fringe-mode))))

(Then "^frame is undecorated$"
  (lambda ()
    (should (frame-parameter (window-frame (get-buffer-window))
                             'undecorated))))

(Then "^window mode line displays time$"
  (lambda ()
    (should display-time-mode)))

(And "^window mode line time is in \"\\([^\"]+\\)\" format$"
  (lambda (arg)
    (should (string= (format "%S" display-time-string-forms)
                     (format "%S" `((propertize
                                     (concat ,(intern arg) ":" minutes))))))))
