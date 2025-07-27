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

(When "^I press \"\\([^\"]+\\)\" without queries$"
  (lambda (binding)
    (let ((kill-buffer-query-functions nil))
      (When "I press \"%s\"" binding))))

(Then "^point should be at \"\\([^\"]+\\)\"$"
  (lambda (pnt)
    (should (= (cond ((string= "point-min" pnt)
                      (point-min))
                     ((string= "point-max" pnt)
                      (point-max))
                     ((string= "line-beginning" pnt)
                      (line-beginning-position))
                     ((string= "line-end" pnt)
                      (line-end-position))
                     (t (string-to-number pnt)))
               (point)))))

(Then "^multi-term should be in \"\\([^\"]+\\)\" mode$"
  (lambda (mode)
    (with-current-buffer test-buffer
      (cond ((string= "char" mode)
             (should (term-in-char-mode)))
            ((string= "line" mode)
             (should-not (term-in-char-mode)))
            (t (error "Wrong mode: %S" mode))))))
