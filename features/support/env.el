;; -*- lexical-binding: t; -*-
(require 'f)

(defvar support-path
  (f-dirname load-file-name))

(defvar features-path
  (f-parent support-path))

(defvar root-path
  (f-parent features-path))

(add-to-list 'load-path root-path)

;; Ensure that we don't load old byte-compiled versions
(let ((load-prefer-newer t))
  (require 'espuds)
  (require 'ert))

(defvar tests-run 0)
(defvar test-buffer nil)

;; (Setup)

(Given "^test buffer is \"\\([^\"]+\\)\"$"
  (lambda (name)
    (when test-buffer
      (error "Test buffer was already set, bad cleanup!"))
    (setq test-buffer name)
    (should test-buffer)
    (get-buffer-create test-buffer)))

(Given "^test buffer is multi-term$"
  (lambda ()
    (when test-buffer
      (error "Test buffer was already set, bad cleanup!"))
    (setq test-buffer (buffer-name (car (last multi-term-buffer-list))))
    (should test-buffer)))

(When "^emacs reads output$"
  (lambda ()
    (let* ((default 0.3)
           (env-timeout (getenv "TIMEOUT"))
           (timeout (if env-timeout (string-to-number env-timeout) default)))
      (accept-process-output nil timeout))))

(Before)

(After
 (setq tests-run (1+ tests-run))
 (when test-buffer
   (kill-buffer (get-buffer-create test-buffer)))
 (setq test-buffer nil)
 (should-not test-buffer))

;; (Teardown)
