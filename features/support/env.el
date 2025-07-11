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

;; (Setup)

(Before)

(After
 (setq tests-run (1+ tests-run)))

;; (Teardown)
