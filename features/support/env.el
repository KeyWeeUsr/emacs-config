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

(Before)

(After
 (setq tests-run (1+ tests-run))
 (when test-buffer
   (kill-buffer (get-buffer-create test-buffer)))
 (setq test-buffer nil)
 (should-not test-buffer))

;; (Teardown)
