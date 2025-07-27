;; -*- lexical-binding: t; -*-
(Before)

(After)

(Given "^selected buffer is \"\\([^\"]+\\)\"$"
  (lambda (buff-name)
    (let ((buff (get-buffer buff-name)))
      (display-buffer buff)
      (select-window (get-buffer-window buff)))))

(Then "^selected buffer should be \"\\([^\"]+\\)\"$"
  (lambda (buff-name)
    (should (string= buff-name
                     (buffer-name (window-buffer (selected-window)))))))

(When "^I press \"\\([^\"]+\\)\" in buffer \"\\([^\"]+\\)\" without queries$"
  (lambda (binding buff-name)
    (let ((kill-buffer-query-functions nil))
      (When "I press \"%s\" in buffer \"%s\"" binding buff-name))))
