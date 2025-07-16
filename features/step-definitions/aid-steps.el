;; -*- lexical-binding: t; -*-
(When "^emacs loads$"
  (lambda ()
    (should after-init-time)
    (require 'elpaca)
    (should elpaca-after-init-time)))

(defvar aid-mock-calls nil)
(defun aid-browse-mock (url)
  (setq aid-mock-calls (push url aid-mock-calls)))
(Before
 (setq aid-mock-calls nil)
 (advice-add 'browse-url :override #'aid-browse-mock
             '((name . ecukes-test-aid-browse-url))))

(After
 (dolist (sym '(read-string))
   (advice-mapc (lambda (func props)
                  (when (string-prefix-p "ecukes-test-aid-"
                                         (format "%s" (alist-get 'name props)))
                    (advice-remove sym func)))
                sym)))

(When "^point is at \"\\([^\"]+\\)\"$"
  (lambda (pnt)
    (unless test-buffer (error "Missing test buffer"))
    (with-current-buffer (get-buffer test-buffer)
      (if (string= "point-max" pnt)
          (setq pnt (point-max))
        (setq pnt (string-to-number pnt)))
      (goto-char pnt))))

(When "^point in multi-term buffer is at \"\\([^\"]+\\)\"$"
  (lambda (pnt)
    (with-current-buffer (car (last multi-term-buffer-list))
      (if (string= "point-max" pnt)
          (setq pnt (point-max))
        (setq pnt (string-to-number pnt)))
      (goto-char pnt))))

(When "^I select previous word$"
  (lambda ()
    (unless test-buffer (error "Missing test buffer"))
    (let ((buff (get-buffer test-buffer)))
      (with-current-buffer buff
        (display-buffer buff)
        (with-selected-window (get-buffer-window buff)
          ;; note: requires display-buffer & with-selected-window
          (execute-kbd-macro (kbd "M-S-b")))))))

(Then "^mark should be active$"
  (lambda ()
    (unless test-buffer (error "Missing test buffer"))
    (with-current-buffer (get-buffer test-buffer)
      (should mark-active))))

(Then "^region should be active$"
  (lambda ()
    (unless test-buffer (error "Missing test buffer"))
    (with-current-buffer (get-buffer test-buffer)
      (should (region-active-p)))))

(Then "^active region should select \"\\([^\"]+\\)\"$"
  (lambda (region)
    (unless test-buffer (error "Missing test buffer"))
    (let ((buff (get-buffer test-buffer)))
      (with-current-buffer buff
        (display-buffer buff)
        (with-selected-window (get-buffer-window buff)
          (should transient-mark-mode)
          ;; note: requires display-buffer & with-selected-window
          (should (use-region-p))
          (should (string= region (format "%s" (region-bounds)))))))))

(When "^I type \"\\([^\"]+\\)\"$"
  (lambda (text)
    (unless test-buffer (error "Missing test buffer"))
    (let ((buff (get-buffer test-buffer)))
      (with-current-buffer buff
        (display-buffer buff)
        (with-selected-window (get-buffer-window buff)
          ;; note: requires display-buffer & with-selected-window
          (execute-kbd-macro text))))))

;; (When "^buffer contains \"\\([^\"]+\\)\":$"
;;   (lambda (buff-name contents data)
;;     (unless test-buffer (error "Missing test buffer"))
;;     (let ((header (car data)))
;;       (should (string-match-p (nth 0 header) buff-name))
;;       (should (string-match-p (nth 1 header) contents)))

;;     (dolist (item (cdr data))
;;       (let ((buff-name (nth 0 item))
;;             (contents (nth 1 item)))
;;         (with-current-buffer (get-buffer-create buff-name)
;;           (insert (string-replace "\\n" "\n" contents)))))))

(Then "^lighter at \"\\([^\"]+\\)\" should show \"\\([^\"]+\\)\":$"
  (lambda (pnt rowcol data)
    (unless test-buffer (error "Missing test buffer"))
    (let ((header (car data)))
      (should (string-match-p (nth 0 header) pnt))
      (should (string-match-p (nth 1 header) rowcol)))

    (let ((buff (get-buffer test-buffer)))
      (with-current-buffer buff
        (display-buffer-in-side-window buff)

        (with-selected-window (get-buffer-window buff)
          (dolist (item (cdr data))
            (force-mode-line-update t)

            (let ((pnt (nth 0 item))
                  (rowcol (nth 1 item)))
              (if (string= "point-max" pnt)
                  (setq pnt (point-max))
                (setq pnt (string-to-number pnt)))
              (goto-char pnt)
              (if (= pnt 0) (should (= 1 (point))) (should (= pnt (point))))

              (let ((text (substring-no-properties
                           (format-mode-line mode-line-position))))
                (if (string-match "(\\([0-9]+\\),\\([0-9]+\\))" text)
                    (should (string= rowcol (match-string 0 text)))
                  (error "should never happen: %S" text))))))))))

(Given "^buffer contains \"\\([^\"]*\\)\"$"
  (lambda (contents)
    (unless test-buffer (error "Missing test buffer"))
    (with-current-buffer (get-buffer test-buffer)
      (insert (string-replace "\\n" "\n" contents)))))

(Then "^buffer should contain \"\\([^\"]+\\)\"$"
  (lambda (contents)
    (unless test-buffer (error "Missing test buffer"))
    (with-current-buffer (get-buffer test-buffer)
      (should (string= (string-replace "\\n" "\n" contents)
                       (buffer-string))))))

(Then "^minor mode \"\\([^\"]+\\)\" should be active$"
  (lambda (mode-name)
    (unless test-buffer (error "Missing test buffer"))
    (should (member 'delete-selection-mode minor-mode-list))))

(Then "^buffer in show-paren-mode should be highlighted by \"\\([^\"]+\\)\"$"
  (lambda (highlight)
    (unless test-buffer (error "Missing test buffer"))
    (let ((buff (get-buffer test-buffer)))
    (with-current-buffer buff
      (dolist (loc-point-color (string-split highlight ","))
        (setq loc-point-color (string-split loc-point-color ":"))
        (goto-char (string-to-number (car loc-point-color)))

        (should show-paren-mode)
        (should (catch 'found
                  (dolist (item timer-idle-list)
                    (when (eq 'show-paren-function (aref item 5))
                      (throw 'found t)))))
        ;; note(idle): tricky to trigger
        (show-paren-function)

        ;; example overlay list returned:
        ;; (#<overlay from 1 to 2 in left-open-missing>)
        (let* ((overlays
                (overlays-at (string-to-number (cadr loc-point-color)))))
          (unless overlays
            (error "should never happen: %s, %s" buff (point)))
          (should (string= (format "show-paren-%s" (caddr loc-point-color))
                           (overlay-get (car overlays) 'face)))))))))

(Given "^multi-term terminal launches$"
  (lambda ()
    (multi-term)))

(Given "^multi-term buffer contains \"\\([^\"]*\\)\"$"
  (lambda (contents)
    (with-current-buffer (car (last multi-term-buffer-list))
      (accept-process-output nil 1)
      (let ((inhibit-read-only t))
        (save-excursion
          (term-mode)
          (goto-char (point-min))
          (insert (string-replace "\\n" "\n" contents))))
      (term-char-mode)
      (accept-process-output nil 1))))

(When "^I press \"\\([^\"]+\\)\" in buffer \"\\([^\"]+\\)\"$"
  (lambda (binding buff-name)
    (let ((buff-target buff-name))
      (cond ((string= "multi-term" buff-name)
             (setq buff-target (car (last multi-term-buffer-list))))
            (t (get-buffer buff-name)))
      (with-current-buffer buff-target
        (should (term-in-char-mode))
        (should (fboundp 'my-open-pr))
        (should (lookup-key term-raw-map (kbd binding)))
        (should (lookup-key (current-local-map) (kbd binding)))
        (should (key-binding (kbd binding) nil t))
        (execute-kbd-macro (kbd binding))))))

(Then "^browser should open \"\\([^\"]+\\)\" url$"
  (lambda (arg)
    (should (string= arg (car aid-mock-calls)))
    (should (= 1 (length aid-mock-calls)))))

(Then "^no multi-term buffer should remain open$"
  (lambda ()
    (accept-process-output nil 1)
    (should-not multi-term-buffer-list)))

(Given "^mode \"\\([^\"]+\\)\" is active$"
  (lambda (mode)
    (unless test-buffer (error "Missing test buffer"))
    (let ((allowed '(org-mode)))
      (setq mode (intern mode))
      (should (member mode allowed))
      (with-current-buffer (get-buffer test-buffer)
        (funcall mode)))))

(Given "^advice for user input returns \"\\([^\"]+\\)\"$"
  (lambda (input)
    (advice-add 'read-string :override (lambda (&rest _) "ASK")
                '((name . ecukes-test-aid-read-string)))))


(Then "^shortcut \"\\([^\"]+\\)\" should become \"\\([^\"]+\\)\":$"
  (lambda (contents result data)
    (unless test-buffer (error "Missing test buffer"))
    (let ((header (car data)))
      (should (string-match-p (nth 0 header) contents))
      (should (string-match-p (nth 1 header) result)))

    (dolist (item (cdr data))
      (let ((contents (nth 0 item))
            (result (string-replace "\\n" "\n" (nth 1 item)))
            (buff (get-buffer-create test-buffer))
            (point-loc ))
        (with-current-buffer buff
          (display-buffer buff)
          (with-selected-window (get-buffer-window buff)
            (insert (string-replace "\\n" "\n" contents))
            (should (= (point-max) (point)))
            (should (featurep 'org-tempo))
            (should-not (eq 'fundamental-mode major-mode))
            (execute-kbd-macro (kbd "TAB"))
            (let ((case-fold-search nil))
              ;; note: point index is 1-based, match is 0-based
              (should (= (1+ (string-match "P" result nil t)) (point))))
            (should (string= (string-replace "P" "" result) (buffer-string)))
            (erase-buffer)))))))
