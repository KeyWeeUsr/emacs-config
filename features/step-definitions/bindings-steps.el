;; -*- lexical-binding: t; -*-
(Before)

(defvar bindings-mock-calls nil)
;; todo: getting ## ("%S") as an argument, can't simplify and check properly
(defun bindings-helpful-callable (sym)
  (setq bindings-mock-calls (push "callable" bindings-mock-calls)))
(defun bindings-helpful-variable (var)
  (setq bindings-mock-calls (push "variable" bindings-mock-calls)))
(defun bindings-helpful-key (key)
  (setq bindings-mock-calls (push "key" bindings-mock-calls)))
(defun bindings-helpful-command (cmd)
  (setq bindings-mock-calls (push "command" bindings-mock-calls)))
(defun bindings-shrink-window (arg)
  (setq bindings-mock-calls (push "shrink-window" bindings-mock-calls)))
(defun bindings-enlarge-window (arg)
  (setq bindings-mock-calls (push "enlarge-window" bindings-mock-calls)))
(defun bindings-shrink-window-horizontally (arg)
  (setq bindings-mock-calls (push "shrink-window-horizontally"
                                  bindings-mock-calls)))
(defun bindings-enlarge-window-horizontally (arg)
  (setq bindings-mock-calls (push "enlarge-window-horizontally"
                                  bindings-mock-calls)))
(Before
 (setq bindings-mock-calls nil)
 (advice-add 'helpful-callable :override #'bindings-helpful-callable
             '((name . ecukes-test-bingings-helpful-callable)))
 (advice-add 'helpful-variable :override #'bindings-helpful-variable
             '((name . ecukes-test-bingings-helpful-variable)))
 (advice-add 'helpful-key :override #'bindings-helpful-key
             '((name . ecukes-test-bingings-helpful-key)))
 (advice-add 'helpful-command :override #'bindings-helpful-command
             '((name . ecukes-test-bingings-helpful-command)))

 (advice-add 'shrink-window :override #'bindings-shrink-window
             '((name . ecukes-test-bingings-shrink-window)))
 (advice-add 'enlarge-window :override #'bindings-enlarge-window
             '((name . ecukes-test-bingings-enlarge-window)))
 (advice-add 'shrink-window-horizontally
             :override #'bindings-shrink-window-horizontally
             '((name . ecukes-test-bingings-shrink-window-horizontally)))
 (advice-add 'enlarge-window-horizontally
             :override #'bindings-enlarge-window-horizontally
             '((name . ecukes-test-bingings-enlarge-window-horizontally))))

(After
 (dolist (name '(callable variable key command))
   (let ((sym (intern (format "helpful-%s" name))))
     (advice-mapc
      (lambda (func props)
        (when (string-prefix-p "ecukes-test-bindings-"
                               (format "%s" (alist-get 'name props)))
          (advice-remove sym func)))
      sym))))

(Given "^selected buffer is \"\\([^\"]+\\)\"$"
  (lambda (buff-name)
    (let ((buff (get-buffer buff-name)))
      (display-buffer buff)
      (select-window (get-buffer-window buff))
      (setq test-buffer-window-properties
            (cddr (window-state-get (selected-window)))))))

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

(When "^I press \"\\([^\"]+\\)\" in minibuffer$"
  (lambda (binding)
    (with-minibuffer-selected-window
      (execute-kbd-macro (kbd binding)))))

(Then "^helpful should open \"\\([^\"]+\\)\"$"
  (lambda (arg)
    (should (string= arg (car bindings-mock-calls)))
    (should (= 1 (length bindings-mock-calls)))))

(Then "^window should change size with \"\\([^\"]+\\)\"$"
  (lambda (arg)
    (should (string= arg (car bindings-mock-calls)))
    (should (= 1 (length bindings-mock-calls)))))
