;; -*- lexical-binding: t; -*-
(When "^I press \"\\([^\"]+\\)\" in test buffer$"
  (lambda (binding)
    (with-current-buffer test-buffer
      (display-buffer test-buffer)
      (with-selected-window (get-buffer-window test-buffer)
        (execute-kbd-macro (kbd binding))))))

(Given "^local variable \"\\([^\"]+\\)\" is \"\\([^\"]*\\)\"$"
  (lambda (name raw-value)
    (let (value)
      (cond ((string-match "\\((.*)\\)" raw-value)
             (if (string= raw-value (match-string 1 raw-value))
                 (setq value (read raw-value))
               (setq value raw-value)))
            ((string-match "\\([0-9]+\\)" raw-value)
             (if (string= raw-value (match-string 1 raw-value))
                 (setq value (string-to-number raw-value))
               (setq value raw-value)))
            (t (setq value raw-value)))
      (with-current-buffer test-buffer
        (set (make-local-variable (intern name)) value)))))

(Before)

(After)
