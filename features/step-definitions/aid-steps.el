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
 (advice-add 'browse-url :override #'aid-browse-mock))
(After
 (advice-remove 'browse-url #'aid-browse-mock))

(When "^point in buffer \"\\([^\"]+\\)\" is at \"\\([^\"]+\\)\"$"
  (lambda (buff-name pnt)
    (let ((buff-target buff-name))
      (cond ((string= "multi-term" buff-name)
             (setq buff-target (car (last multi-term-buffer-list))))
            (t (get-buffer buff-name)))
      (with-current-buffer buff-target
        (if (string= "point-max" pnt)
            (setq pnt (point-max))
          (setq pnt (string-to-number pnt)))
        (goto-char pnt)))))

(And "^I select previous word in buffer \"\\([^\"]+\\)\"$"
  (lambda (buff-name)
    (let ((buff (get-buffer buff-name)))
      (with-current-buffer buff
        (display-buffer buff)
        (with-selected-window (get-buffer-window buff)
          ;; note: requires display-buffer & with-selected-window
          (execute-kbd-macro (kbd "M-S-b")))))))

(Then "^mark in temp buffer \"\\([^\"]+\\)\" should be active$"
  (lambda (buff-name)
    (with-current-buffer buff-name
      (should mark-active))))

(Then "^region in temp buffer \"\\([^\"]+\\)\" should be active$"
  (lambda (buff-name)
    (with-current-buffer buff-name
      (should (region-active-p)))))

(Then "^active region in temp buffer \"\\([^\"]+\\)\" should select \"\\([^\"]+\\)\"$"
  (lambda (buff-name region)
    (let ((buff (get-buffer buff-name)))
      (with-current-buffer buff
        (display-buffer buff)
        (with-selected-window (get-buffer-window buff)
          (should transient-mark-mode)
          ;; note: requires display-buffer & with-selected-window
          (should (use-region-p))
          (should (string= region (format "%s" (region-bounds)))))))))

(And "^I type \"\\([^\"]+\\)\" in buffer \"\\([^\"]+\\)\"$"
  (lambda (text buff-name)
    (with-current-buffer (get-buffer buff-name)
      (display-buffer buff-name)
      (with-selected-window (get-buffer-window (get-buffer buff-name))
        ;; note: requires display-buffer & with-selected-window
        (execute-kbd-macro text)))))

(Given "^temp buffer \"\\([^\"]+\\)\" contains \"\\([^\"]+\\)\":$"
  (lambda (buff-name contents data)
    (let ((header (car data)))
      (should (string-match-p (nth 0 header) buff-name))
      (should (string-match-p (nth 1 header) contents)))

    (dolist (item (cdr data))
      (let ((buff-name (nth 0 item))
            (contents (nth 1 item)))
        (with-current-buffer (get-buffer-create buff-name)
          (insert contents))))))

(Then "^lighter in buffer \"\\([^\"]+\\)\" at \"\\([^\"]+\\)\" should show \"\\([^\"]+\\)\":$"
  (lambda (buff-name pnt rowcol data)
    (let ((header (car data)))
      (should (string-match-p (nth 0 header) pnt))
      (should (string-match-p (nth 1 header) rowcol)))

    (with-current-buffer (get-buffer-create buff-name)
      (display-buffer-in-side-window (get-buffer buff-name) '((side . bottom)))

      (with-selected-window (get-buffer-window buff-name)
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
              (error "should never happen: %S" text)))))))))

(And "^temp buffer \"\\([^\"]+\\)\" contains \"\\([^\"]*\\)\"$"
  (lambda (buff-name contents)
    (with-current-buffer (get-buffer-create buff-name)
      (insert (string-replace "\\n" "\n" contents)))))

(Then "^temp buffer \"\\([^\"]+\\)\" should contain \"\\([^\"]+\\)\"$"
  (lambda (buff-name contents)
    (with-current-buffer (get-buffer buff-name)
      (should (string= (string-replace "\\n" "\n" contents)
                       (buffer-string))))))

(Then "^minor mode \"\\([^\"]+\\)\" should be activated$"
  (lambda (mode-name)
    (should (member 'delete-selection-mode minor-mode-list))))

(Then "^temp buffer \"\\([^\"]+\\)\" in show-paren-mode should be highlighted by \"\\([^\"]+\\)\":$"
  (lambda (buff-name highlight data)
    (let ((header (car data)))
      (should (string-match-p (nth 0 header) buff-name))
      (should (string-match-p (nth 1 header) highlight)))

    (dolist (item (cdr data))
      (let ((item-buff-name (nth 0 item))
            (item-highlight (nth 1 item)))
        (with-current-buffer (get-buffer item-buff-name)
          ;; (display-buffer-in-side-window (get-buffer item-buff-name) '((side . bottom)))
          ;; (with-selected-window (get-buffer-window item-buff-name)
          ;; (show-paren-mode)
          (dolist (loc-point-color (string-split item-highlight ","))
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
                (error "should never happen: %s, %s" item-buff-name (point)))
              (should (string= (format "show-paren-%s" (caddr loc-point-color))
                               (overlay-get (car overlays) 'face))))))))))

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

(Given "^I press \"\\([^\"]+\\)\" in buffer \"\\([^\"]+\\)\"$"
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

(And "^mode \"\\([^\"]+\\)\" is activated$"
  (lambda (arg)

    ))

(And "^advice for \"\\([^\"]+\\)\" returns \"\\([^\"]+\\)\"$"
  (lambda (arg-1 arg-2)

    ))

(Then "^inserting \"\\([^\"]+\\)\" and pressing \"\\([^\"]+\\)\" should convert to:$"
  (lambda (arg-1 arg-2 arg-3)

    ))
