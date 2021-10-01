;; .emacs
;;
;; Copyright (C) 2016 - 2021, KeyWeeUsr(Peter Badida) <keyweeusr@gmail.com>
;;
;; Author: KeyWeeUsr
;; URL: https://github.com/KeyWeeUsr/emacs-config
;; Version: 1.0
;; License: MIT
;;

;; This file is not part of GNU Emacs

;; Allow emacsclient -n file.ext opening
(server-start)

;; MELPA repo + cl-lib
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list
   'package-archives
   '("gnu" . "http://elpa.gnu.org/packages/")
   )
  )
(package-initialize)
(if (file-exists-p "~/.emacs.d/_touch_KeyWeeUsr.txt") nil
  (package-refresh-contents)
  (write-region "" "" "~/.emacs.d/_touch_KeyWeeUsr.txt")
  )

;; Packages
;; Install "use-package"
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)
      )
  )
(require 'use-package)

;; package-install [RET] <package> [RET]
(use-package auto-complete :ensure auto-complete)
(use-package langdoc :ensure langdoc)
(use-package ascii-art-to-unicode :ensure ascii-art-to-unicode)
(use-package bind-key :ensure bind-key)
(use-package brainfuck-mode :ensure brainfuck-mode)
(use-package diminish :ensure diminish)
(use-package popup :ensure popup)
(use-package php-mode :ensure php-mode)
(use-package gradle-mode :ensure gradle-mode)

;; Initialize available packages
(package-initialize)

;; Separate plugins folder
(unless (file-exists-p "~/.emacs.d/plugins")
  (make-directory "~/.emacs.d/plugins")
  )
(add-to-list 'load-path "~/.emacs.d/plugins")

;; Kivy .kv syntax plugin
;; Download if not available
(if (file-exists-p "~/.emacs.d/plugins/kivy-mode.el") nil
  (setq _GH_RAW "https://raw.githubusercontent.com/")
  (setq _KV_MODE "kivy/kivy/master/kivy/tools/highlight/kivy-mode.el")
  (url-copy-file
   (concat _GH_RAW _KV_MODE)
   "~/.emacs.d/plugins/kivy-mode.el")
  )

;; Allow kivy-mode in emacs
(require 'kivy-mode)
(add-to-list 'auto-mode-alist '("\\.kv$" . kivy-mode))
(add-hook 'kivy-mode-hook
 '(lambda ()
    (electric-indent-local-mode t)))

;; allow dead-acute + ibus for japanese
(require 'iso-transl)

;; Customizing
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(c-basic-indent 4)
 '(c-basic-offset 4)
 '(c-default-style "k&r")
 '(c-offsets-alist (quote ((arglist-close c-lineup-close-paren))))
 '(c-set-offset (quote arglist-close) t)
 '(column-number-mode t)
 '(custom-enabled-themes (quote (wombat)))
 '(delete-selection-mode t)
 '(desktop-save-mode t)
 '(ede-project-directories (quote ("/tmp")))
 '(fill-column 79)
 '(fringe-mode nil nil (fringe))
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(js-indent-level 4)
 '(org-startup-truncated nil)
 '(org-support-shift-select t)
 '(package-selected-packages
   (quote
    (wc-mode aas ## csharp-mode org-d20 yaml-mode rjsx-mode flyspell-correct poly-rst 2048-game gradle-mode php-mode diminish brainfuck-mode ascii-art-to-unicode langdoc auto-complete use-package)))
 '(save-place t nil (saveplace))
 '(scroll-bar-mode nil)
 '(sgml-basic-offset 4)
 '(show-paren-mode t)
 '(tab-stop-list (number-sequence 4 200 4))
 '(tab-width 4)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(trailing-whitespace ((t (:background "red1")))))

;; CUSTOM FUNCTIONS & BINDINGS
;; Functions to auto-complete-mode everywhere
(require 'auto-complete)

(defun acon ()
  "Enable autocomplete in ALL buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (auto-complete-mode 1)
      (electric-pair-mode 1)
      )))
(defun acoff ()
  "Disable autocomplete in ALL buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (auto-complete-mode -1)
      (electric-pair-mode -1)
      )))

;; View resizing
(global-set-key (kbd "C-c <up>") 'shrink-window)
(global-set-key (kbd "C-c <down>") 'enlarge-window)
(global-set-key (kbd "C-c <left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-c <right>") 'enlarge-window-horizontally)

;; Add <backtab> unindenting
;;(global-set-key (kbd "<backtab>") (read-kbd-macro "<backspace>"))
;; ref https://stackoverflow.com/a/2250155/5994041
(global-set-key (kbd "<backtab>") 'un-indent-by-removing-4-spaces)
(defun un-indent-by-removing-4-spaces ()
  "remove 4 spaces from beginning of of line"
  (interactive)
  (save-excursion
    (save-match-data
      (beginning-of-line)
      ;; get rid of tabs at beginning of line
      (when (looking-at "^\\s-+")
        (untabify (match-beginning 0) (match-end 0)))
      (when (looking-at (concat "^" (make-string tab-width ?\ )))
        (replace-match "")))))
;; end ref

(add-hook 'bat-mode-hook
          '(lambda ()
             ;; Indent with spaces properly
             ;; (no weird indenting by function/brackets)
             (setq indent-line-function 'tab-to-tab-stop)

             ;; Stop the stupid electric indentation of a plain \n!
             (electric-indent-local-mode -1)
             )
          )
(put 'downcase-region 'disabled nil)
