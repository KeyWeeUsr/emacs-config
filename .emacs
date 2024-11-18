;;; .emacs --- Emacs config -*- lexical-binding: t; -*-

;; Copyright (C) 2016 - 2024, KeyWeeUsr(Peter Badida) <keyweeusr@gmail.com>

;; Author: KeyWeeUsr
;; Version: 4.4

;; (use-package)
;; Package-Requires: ((emacs "29.1"))

;; (let)
;; Package-Requires: ((emacs "24.4"))

;; Homepage: https://github.com/KeyWeeUsr/emacs-config
;; License: MIT

;;; Commentary:

;; TBD

;;; Code:

;; This file is not part of GNU Emacs

;; Allow emacsclient -n file.ext opening
(server-start)

;; Customizing
;; fill-column, indent-tabs-mode, indicate-empty-lines, ring-bell-function
;; tab-stop-list, tab-width, tool-bar-mode
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(wombat))
 '(fill-column 79)
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(ispell-dictionary nil)
 '(package-selected-packages
   '(httprepl dbml-mode kivy-mode syncthing keepass-mode ace-window curl-to-elisp ecukes dedicated docker-compose-mode exec-path-from-shell digit-groups graphviz-dot-mode cmake-mode yaml-mode which-key wc-mode v-mode use-package undercover typewriter-roll-mode terraform-mode ssh-config-mode php-mode package-lint osm org-web-tools org-transclusion org-roam-ui org-roam-timestamps org-epa-gpg ob-base64 multi-term mermaid-docker-mode markdown-mode htmlize helpful gradle-mode go-mode gnu-elpa-keyring-update ess elfeed-tube-mpv elfeed-org dockerfile-mode dired-duplicates diminish define-it decor cython-mode company-quickhelp brainfuck-mode auto-complete ascii-art-to-unicode))
 '(ring-bell-function 'ignore)
 '(scroll-bar-mode nil)
 '(show-paren-mode t nil (paren))
 '(tab-stop-list (number-sequence 4 200 4))
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(use-file-dialog nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(minibuffer-prompt ((t (:foreground "#e5786d" :height 0.9))))
 '(mode-line ((t (:background "#444444" :foreground "#f6f3e8" :height 0.6))))
 '(mode-line-inactive ((t (:background "#444444" :foreground "#857b6f" :height 0.6))))
 '(term-color-blue ((t (:background "deep sky blue" :foreground "deep sky blue"))))
 '(trailing-whitespace ((t (:background "red1")))))
(put 'upcase-region 'disabled nil)
(put 'list-timers 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'downcase-region 'disabled nil)

(defun migdate ()
  "Current date for file migrations."
  (interactive)
  (shell-command "date \"+%Y%m%d%H%M%S\""))

(defun my-open-pr ()
  "Follow a link from git remote to open a PR."
  (interactive)
  (save-excursion
    (unwind-protect
        (progn
          (term-mode)
          (re-search-backward "Create a pull request")
          (re-search-forward "http") (re-search-backward "http")
          (unwind-protect
              (progn
                (push-mark)
                (re-search-forward "^remote:") (re-search-backward "^remote:")
                (backward-char 1)
                (browse-url
                 (string-replace "\n" "" (buffer-substring-no-properties
                                          (mark) (point)))))
            (pop-mark)))
      (term-char-mode))))

(defun my-move (left-top)
  "Move window to comma-separated LEFT-TOP location."
  (interactive "sLeft,Top: ")
  (let* ((left (string-to-number
                (car (split-string left-top "," nil))))
         (top (string-to-number
               (cadr (split-string left-top "," nil)))))
    (modify-frame-parameters (window-frame)
                             `((left . ,left) (top . ,top)))))
;; Fixes for MacOS
(when (eq window-system 'ns)
  (with-no-warnings
    (setq ns-right-alternative-modifier 'none))
  (setq mac-right-option-modifier 'none)
  (advice-add 'ns-print-buffer :override (lambda (&rest _))
              '((name . "mac-keyboard")))
  (setq exec-path (append '("/opt/homebrew/bin") exec-path)))

(defun my-fix-mac ()
  "Fix Emacs breaking on MacOS."
  (interactive)
  (when (eq window-system 'ns)
    (tool-bar-mode -1)
    (setq frame-resize-pixelwise t)
    (dotimes (_ 3)
      (toggle-frame-maximized))))

(defun custom-exit ()
  "Run at exit."
  (interactive)
  (progn
    (progn
      (message "Deleting MPV playlist")
      (delete-file "/tmp/mpvplaylist")
      t)
    (progn
      (message "Deleting org-roam DB")
      (when (boundp 'org-roam-db-location)
        (delete-file org-roam-db-location)
        (if (file-exists-p org-roam-db-location)
            (progn
              (message "Failed to delete org-roam DB, delete manually!")
              (sleep-for 5)
              nil)
          (progn
            (message "Successfully deleted org-roam DB")
            (sleep-for 0.1)
            t))))))
(unless (string= window-system "android")
  (add-hook 'kill-emacs-query-functions 'custom-exit))

;; MELPA repo + cl-lib
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(when (< emacs-major-version 24)
  (add-to-list 'package-archives
               '("gnu" . "http://elpa.gnu.org/packages/")))

;; Exploits and other fixes
;; 1) LaTeX & Org exploit fix
(when (and (< emacs-major-version 29)
           (< emacs-minor-version 3))
  (setq org-preview-latex-default-process 'verbatim))

;; Initialize available packages
(unless (< emacs-major-version 27)
  (package-initialize))
(unless (fboundp 'package-activate-all)
  (warn "This should be covered by < 27 check!")
  (package-initialize))

;; Packages
;; always refresh beforehand
(package-refresh-contents)
;; Install "use-package"
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

(defun my-cache-gpg-key (&optional keep ctx)
  (interactive)
  (unless ctx (setq ctx (epg-make-context)))
  (setf (epg-context-pinentry-mode ctx) 'loopback)

  (if (epg-sign-string ctx "\n") (message "Pre-loaded GPG key")
    (warn "Pre-loading GPG key failed"))

  (when keep (setq my-cache-gpg-key-timer
                   (run-with-timer 1 290 #'my-cache-gpg-key))))

(defun my-cache-gpg-progress
    (context operation display-chr current total data)
  "From epg-context-set-progress-callback."
  (when (= current total)
    (run-at-time
     2 nil (lambda (&rest _)
             (setq kill-ring (cdr kill-ring))
             (when (display-graphic-p) (gui-select-text "ok"))
             (message "Clipboard cleared")))))

(defun my-string-or (what default)
  (if (string= "" what) default what))

(defun my-keepass-init ()
  (interactive)
  (cancel-function-timers 'my-cache-gpg-key)
  (setq my-cache-gpg-key-timer nil)
  (let* ((user (user-real-login-name))
         (db-path (read-file-name-default "KeePass DB: " nil ""))
	     (keepass-mode-db (unless (string= "" db-path)
                            (expand-file-name db-path)))
	     (keepass-mode-password
          (when keepass-mode-db (read-passwd "KeePass password: ")))
         (const-username "UserName")
         (const-password "Password")
         (const-notes "Notes"))
    (fset 'cred `(lambda (what type)
                   (keepass-mode-get
                    type (format "emacs-creds/%s-%s" what ,user))))
    (require 'subr-x)
    (setq syncthing-default-server-token
          (when keepass-mode-db (cred "syncthing-token" const-password)))
    (setq my-imgur-client-id
          (when keepass-mode-db (cred "imgur-api" const-username)))
    (setq my-imgur-client-secret
          (when keepass-mode-db (cred "imgur-api" const-password)))
    (setq syncthing-default-server-token
          (when keepass-mode-db (cred "syncthing-token" const-password)))
    ;; Only unless found set to default, otherwise nil
    (setq elfeed-db-directory
          (when keepass-mode-db
            (my-string-or (cred "elfeed-db-dir" const-username) "~/elfeed")))
    ;; Only unless found set to default, otherwise nil
    (setq my-org-roam-directory
          (when keepass-mode-db
            (my-string-or (cred "org-roam-dir" const-username)
                          (expand-file-name "roam" user-emacs-directory))))
    (setq my-org-roam-templates
          (when keepass-mode-db
            (mapcar (lambda (item) (split-string item ";"))
                    (split-string
                     (cred "org-roam-templates" const-notes) ";;"))))
    (setq my-epa-file-encrypt-to
          (when keepass-mode-db (cred "epa-file-encrypt-to" const-username)))
    (setq my-gpg-id (when keepass-mode-db (cred "gpg-main" const-notes)))
    (let ((tmp nil) (epg-user-id my-gpg-id) (ctx (epg-make-context))
          (select-enable-clipboard t) (select-enable-primary t))
      (epg-context-set-progress-callback ctx #'my-cache-gpg-progress)
      (unwind-protect
          (when keepass-mode-db
            (setq tmp (cred "gpg-main" const-password))
            ;; trim kill-ring
            (setq kill-ring
                  (butlast kill-ring
                           (- (length kill-ring) (1- kill-ring-max))))
            ;; cleared in progress func
            (kill-new tmp)
            (when (display-graphic-p)
              (gui-select-text tmp))
            (condition-case err (my-cache-gpg-key t ctx)
              (t (warn "Pre-loading GPG key failed (%s)" err))))))
    (makunbound 'cred))
  (when (or (not (boundp 'my-gpg-id)) (null my-gpg-id))
    (warn "KeePass init failed")))

(use-package keepass-mode
  :ensure t
  :config
  (unless (eq window-system 'android)
    (my-keepass-init)))

(use-package syncthing
  :ensure t
  :config
  (progn
    (setq syncthing-header-items
          '("rate-download" "rate-upload" "count-local-files"
            "count-local-folders" "count-local-bytes" "count-listeners"
            "count-discovery" "uptime" "my-id" "version"))))

;; Functions to auto-complete-mode everywhere
(defun ac-onoff (flag)
  "Toggle function `auto-complete-mode' in all buffers with boolean FLAG."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (auto-complete-mode flag) (electric-pair-mode flag))))
(defun acon ()
  "Enable autocomplete in ALL buffers."
  (interactive) (ac-onoff 1))
(defun acoff ()
  "Disable autocomplete in ALL buffers."
  (interactive) (ac-onoff -1))

;; possibly unnecessary
(use-package ansi-color
  :ensure t
  :config
  (progn
    (setq ansi-color-faces-vector
          [default default default italic underline success warning error])))
;; render ansi escape colors
(defun ansi-color-region()
  "Interactive version of func."
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(use-package cc-mode
  :ensure t
  :config
  (progn
    (setq c-basic-indent 4) ;; TODO: does not exist?
    (setq c-basic-offset 4)
    (setq c-default-style "k&r")
    (setq c-offsets-alist '((arglist-close . +)))
    (setq c-set-offset 'arglist-close)))
(use-package simple
  :ensure nil  ;; bundled in Emacs
  :config (column-number-mode t))
(use-package delsel
  :ensure t
  :config (delete-selection-mode t))
(use-package auto-complete
  :ensure t
  :config
  (progn
    (ac-config-default)
    (setq ac-use-menu-map t)
    (advice-add 'create-file-buffer
                :after (lambda (&rest _) (acon)))))
(use-package desktop
  :ensure t
  :config (desktop-save-mode t))
(use-package dired
  :ensure nil  ;; bundled in Emacs
  :config (setq dired-listing-switches "-al"))
(use-package fringe
  :ensure nil  ;; bundled in Emacs
  :config (fringe-mode 8))
(use-package files
  :ensure nil  ;; bundled in Emacs
  :config
  (setq safe-local-variable-values
        '((org-table-convert-region-max-lines . 2000)
          (eval ispell-change-dictionary "greek")
          (ispell-dictionary . greek-hunspell)
          (ispell-dictionary . greek)
          (eval setq-local load-path
                (push default-directory load-path))
          (org-confirm-babel-evaluate)
          (org-babel-python-command . "~/envs/selenium/bin/python")
          (latex-run-command . "pdflatex -shell-escape")
          (org-preview-latex-default-process . dvipng)
          (org-hierarchical-todo-statistics . t)
          (ispell-dictionary . "slovak")
          (eval progn
                (flyspell-mode)
                (typewriter-roll-mode))
          (eval face-remap-add-relative 'default :family "Serif" :height 1.3)
          (eval progn
                (flyspell-mode)
                (wc-mode))
          (eval quote flymake-mode)
          (set . fill-column=79))))
(use-package ispell
  :ensure t
  :config (setq ispell-dictionary nil))
(use-package js
  :ensure t
  :config (setq js-indent-level 4))
(use-package nxml-mode
  :ensure nil  ;; bundled in Emacs
  :config
  (progn
    (setq nxml-attribute-indent 4)
    (setq nxml-child-indent 4)))
(use-package saveplace
  :ensure nil  ;; bundled in Emacs
  :config (setq save-place-mode t))
(use-package sgml-mode
  :ensure nil  ;; bundled in Emacs
  :config (setq sgml-basic-offset 4))
(use-package langdoc :ensure t)
(use-package ascii-art-to-unicode :ensure t)
(use-package bind-key :ensure t)
(use-package brainfuck-mode :ensure t)
(use-package diminish :ensure t)
(use-package popup :ensure t)
(use-package php-mode :ensure t)
(use-package gradle-mode :ensure t)
(use-package ssh-config-mode :ensure t)
(use-package v-mode :ensure t)
(use-package term
  :ensure t
  :config
  (progn
    (setq term-buffer-maximum-size 0)))
(use-package multi-term
  :ensure t
  :after term
  :config
  (progn
    (setq term-bind-key-alist
          '(("C-c C-c" . term-interrupt-subjob)
            ("C-c C-e" . term-send-esc)
            ("C-p" . previous-line)
            ("C-n" . next-line)
            ("C-s" . isearch-forward)
            ("C-r" . isearch-backward)
            ("C-m" . term-send-return)
            ("C-y" . term-paste)
            ("M-f" . term-send-forward-word)
            ("M-b" . term-send-backward-word)
            ("M-o" . term-send-backspace)
            ("M-p" . term-send-up)
            ("M-n" . term-send-down)
            ("M-M" . term-send-forward-kill-word)
            ("M-N" . term-send-backward-kill-word)
            ("<C-backspace>" . term-send-backward-kill-word)
            ("M-r" . term-send-reverse-search-history)
            ("M-d" . term-send-delete-word)
            ("M-," . term-send-raw)
            ("M-." . comint-dynamic-complete)
            ("C-c C-j" . term-mode)
            ("C-c C-k" . term-char-mode)
            ("C-c C-o" . my-open-pr)))))

(use-package wc-mode :ensure t)
(use-package org
  :ensure t
  :config
  (progn
    (setq org-agenda-files nil)
    (setq org-babel-load-languages
          '((emacs-lisp . t)
            (awk . t)
            (shell . t)
            (python . t)
            (latex . t)
            (R . t)
            (dot . t)))
    (org-babel-do-load-languages
     'org-babel-load-languages
     org-babel-load-languages)
    (setq org-edit-src-content-indentation 0)
    (setq org-latex-hyperref-template
          "\\hypersetup{
 pdfauthor={%a},
 pdftitle={%t},
 pdfkeywords={%k},
 pdfsubject={%d},
 pdfcreator={%c},
 pdflang={%L},
 pdfborderstyle={/S/U/W 0.1},
 linkbordercolor={{0 .7 .7}},
 colorlinks=false}")
    (setq org-startup-truncated nil)
    (setq org-support-shift-select t)
    (setq org-src-tab-acts-natively t)))

;; org-roam zettelkasten
(defun my-set-org-roam-directory ()
  "Set `org-roam-directory'."
  (interactive)
  (setq org-roam-directory my-org-roam-directory))
(use-package org-roam
  :ensure t
  :after (keepass-mode org)
  :config
  (progn
    (let ((my-org-roam-filename "%<%Y%m%d%H%M%S>-${id}.org.gpg")
          (my-org-roam-heading
           (format "-*- epa-file-encrypt-to: (%S); fill-column: 50 -*-"
                   my-epa-file-encrypt-to))
          (my-org-roam-template-prefix
           '("#+templateversion: 0" "#+filetags: untagged"
             "#+title: ${title}" "#+startup: fold")))
      (setq org-roam-db-autosync-mode nil)
      (setq org-roam-capture-templates
            `(("d" "default" plain
               ,(string-join `(,@my-org-roam-template-prefix "\n%?") "\n")
               :target
               (file+head ,my-org-roam-filename ,my-org-roam-heading)
               :unnarrowed t)
              ("b" "book notes" plain
               ,(string-join
                 `(,@my-org-roam-template-prefix
                   "" "* Source" "%^{Author}" "Title: ${title}"
                   "Year: %^{Year}" "* Summary" "%?") "\n")
               :target
               (file+head ,my-org-roam-filename ,my-org-roam-heading)
               :unnarrowed t)
              ("v" "video notes" plain
               ,(string-join
                 `(,@my-org-roam-template-prefix
                   "" "* Source" "%^{URL}" "Title: %^{Title}" "Year: %^{Year}"
                   "* Summary" "%?") "\n")
               :target
               (file+head ,my-org-roam-filename ,my-org-roam-heading)
               :unnarrowed t)))
      (my-set-org-roam-directory)))
  :bind (("C-c n f" . my-org-roam-node-find)
         ("C-c n r" . org-roam-node-random)
         ("C-c n z" . org-roam-ext-find-by-tag)
         ("C-c n #" . org-roam-ext-find-by-tag)
         (:map org-mode-map
               (("C-c n i" . org-roam-node-insert)
                ("C-c n o" . org-id-get-create)
                ("C-c n t" . org-roam-tag-add)
                ("C-c n a" . org-roam-alias-add)
                ("C-c n l" . org-roam-buffer-toggle)))))
(use-package org-roam-timestamps
  :ensure t
  :after org-roam
  :config
  (progn
    (setq org-roam-timestamps-minimum-gap 60)
    (org-roam-timestamps-mode)))
(use-package org-roam-ui
  :after org-roam
  :ensure t
  ;; https://github.com/org-roam/org-roam-ui/issues/202
  :init (progn (add-to-list 'desktop-minor-mode-table
                            '(org-roam-ui-mode nil))
               (add-to-list 'desktop-minor-mode-table
                            '(org-roam-ui-follow-mode nil))
               (setq org-roam-ui-open-on-start nil)))
(use-package decor
  :ensure t
  :config (progn (decor-mode)
                 (add-hook 'after-init-hook 'decor-all-frames-off)))
(use-package mermaid-mode
  :ensure t)
(use-package mermaid-docker-mode
  :ensure t
  :after mermaid-mode
  :config (progn (setq mermaid-docker-external-viewer-bin "/usr/bin/xviewer")
                 (setq mermaid-docker-focus-steal-ms 100)))
(use-package typewriter-roll-mode
  :ensure t)

;; Separate plugins folder
(let ((plugins-dir (expand-file-name "plugins" user-emacs-directory)))
  (unless (file-exists-p plugins-dir) (make-directory plugins-dir))
  (add-to-list 'load-path plugins-dir))

;; Kivy .kv syntax plugin
(let* ((name "kivy-mode.el")
       (expected-digest "f876c71caa63c916ed49a78f6c521e28")
       (dest (format "plugins/%s" name))
       (raw "https://raw.githubusercontent.com")
       (mode (format "kivy/kivy/master/kivy/tools/highlight/%s" name))
       (full-dest (expand-file-name dest user-emacs-directory))
       (digest ""))
  (unless (file-exists-p full-dest)
    (url-copy-file (format "%s/%s" raw mode) full-dest))
  (setq digest
        (md5 (with-temp-buffer
               (insert-file-contents-literally full-dest)
               (buffer-substring-no-properties (point-min) (point-max)))))
  (if (not (string= expected-digest digest))
      (warn "kivy-mode digest mismatch: %s != %s" expected-digest digest)
    (use-package kivy-mode :ensure t)
    (add-to-list 'auto-mode-alist '("\\.kv$" . kivy-mode))
    (add-hook 'kivy-mode-hook (lambda () (electric-indent-local-mode t)))))

(use-package iso-transl  ;; allow dead-acute + ibus for japanese
  :ensure nil)  ;; bundled in Emacs

(use-package epa-file  ;; encryption for .gpg files
  :ensure nil  ;; bundled in Emacs
  :init (unless window-system (setf epg-pinentry-mode 'loopback))
  :config (epa-file-enable))
(use-package org-epa-gpg
  :ensure t
  :after epa-file)
(use-package ob-base64
  :ensure t
  :after org
  :config
  (setf (alist-get 'base64 org-babel-load-languages) t))
(use-package dbml-mode
  :ensure t)

;; elfeed customization
(defun elfeed-search-untag-all-unread---reversed ()
  "Reverse 'r' behavior in elfeed."
  (interactive)
  (progn
    (elfeed-search-untag-all-unread)
    (previous-logical-line)
    (previous-logical-line)))

(defun elfeed-search--append-to-mpv-playlist ()
  "Append YouTube video from Elfeed list/search to MPV playlist."
  (interactive)
  (save-window-excursion
    ;; show entry of currently selected/hovered item
    (elfeed-search-show-entry (elfeed-search-selected :ignore-region))
    ;; put to MPV playlist
    (elfeed-tube-mpv (point))
    ;; tag so it doesn't get lost
    (elfeed-show-tag 'in-playlist)
    (elfeed-show-untag 'unread)
    ;; not necessary with playlist tagging
    ;; ;; tmp file because MPV can crash and drop remaining parts
    ;; (save-window-excursion
    ;;   (find-file "/tmp/mpvplaylist")
    ;;   (end-of-buffer)
    ;;   (open-line 1)
    ;;   (forward-char)
    ;;   (insert (shell-command-to-string "mpv-last"))
    ;;   (backward-delete-char-untabify 2)
    ;;   (move-beginning-of-line 1)
    ;;   (delete-char)
    ;;   (move-end-of-line 1)
    ;;   (open-line 1)
    ;;   (forward-char)
    ;;   (save-buffer)
    ;;   (kill-buffer))
    ;; kill elfeed-entry
    (elfeed-kill-buffer)))
(defun elfeed-search--append-to-mpv-playlist---reversed ()
  "Append YouTube video from Elfeed list/search to MPV playlist in reverse."
  (interactive)
  (elfeed-search--append-to-mpv-playlist)
  (forward-line -2))
(defun elfeed-search--open-link ()
  "Open item in browser directly from Elfeed list/search."
  (interactive)
  (elfeed-search-browse-url)
  (when current-prefix-arg
    (forward-line -1)
    (elfeed-search-tag-all-unread)))

(use-package elfeed
  :ensure t
  :config
  (setq elfeed-search-filter "@2023-02-07T23:59 +unread ")
  :bind (:map elfeed-search-mode-map
              ("R" . elfeed-search-untag-all-unread---reversed)
              ("A" . elfeed-search--append-to-mpv-playlist---reversed)
              ("a" . elfeed-search--append-to-mpv-playlist)
              ("o" . elfeed-search--open-link))
  :bind (:map elfeed-show-mode-map
              ("C-c C-o" . elfeed-show-visit)))

;; elfeed in org-mode
(use-package shr
  :ensure t
  :config (setq shr-inhibit-images t))
(use-package elfeed-org
  :ensure t
  :after (elfeed shr)
  :config
  (progn
    (setq rmh-elfeed-org-files (list (format "%s.org" elfeed-db-directory)))
    (elfeed-org)))

;; elfeed youtube
(use-package elfeed-tube
  :ensure t
  :after elfeed-org
  :demand t
  :config
  ;; (setq elfeed-tube-auto-save-p nil) ; default value
  ;; (setq elfeed-tube-auto-fetch-p t)  ; default value
  (progn
    (setq elfeed-tube-fields '(duration description))
    (setq elfeed-tube-mpv-options
          '("--cache=yes" "--ytdl-format=22" "--save-position-on-quit"))
    (elfeed-tube-setup))
  :bind (:map elfeed-show-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)
         :map elfeed-search-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)))
;; elfeed youtube local
(use-package elfeed-tube-mpv
  :ensure t
  :after elfeed-tube
  :bind (:map elfeed-show-mode-map
         ("C-c C-m" . elfeed-tube-mpv)
         ("C-c C-f" . elfeed-tube-mpv-follow-mode)
         ("C-c C-w" . elfeed-tube-mpv-where)))
(use-package terraform-mode
  :ensure t)

;; CUSTOM FUNCTIONS & BINDINGS
(defun m()
  "(multi-term) shortcut."
  (interactive)
  (multi-term))

;; Helpful
(use-package helpful
  :ensure t
  :config
  (progn
    (global-set-key (kbd "C-h f") #'helpful-callable)
    (global-set-key (kbd "C-h v") #'helpful-variable)
    (global-set-key (kbd "C-h k") #'helpful-key)
    (global-set-key (kbd "C-h x") #'helpful-command)))

;; View resizing
(global-set-key (kbd "C-c <up>") 'shrink-window)
(global-set-key (kbd "C-c <down>") 'enlarge-window)
(global-set-key (kbd "C-c <left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-c <right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-c |") 'multi-term-leftside)
(global-set-key (kbd "<f12>") 'kill-buffer-and-window)

;; Add <backtab> unindenting
;;(global-set-key (kbd "<backtab>") (read-kbd-macro "<backspace>"))
;; ref https://stackoverflow.com/a/2250155/5994041
(global-set-key (kbd "<backtab>") 'un-indent-by-removing-4-spaces)
(defun un-indent-by-removing-4-spaces ()
  "Remove 4 spaces from beginning of of line."
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

;; custom code for modes
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-c") 'eval-buffer)))
;; tab-completion for *scratch*
(add-hook 'emacs-lisp-mode-hook
          (lambda () (set (make-local-variable 'tab-always-indent) 'complete)))
(add-hook 'bat-mode-hook
          (lambda ()
            ;; Indent with spaces properly
            ;; (no weird indenting by function/brackets)
            (setq indent-line-function 'tab-to-tab-stop)
            (electric-indent-local-mode -1)))
(add-hook 'sql-mode-hook
          (lambda ()
            ;; Indent with spaces properly
            ;; (no weird indenting by function/brackets)
            (setq indent-line-function 'tab-to-tab-stop)))

;; escape nano w/ C-x
(defun term-send-C-x ()
  "Send `C-x' to terminal."
  (interactive)
  (term-send-raw-string "\C-x"))

;; layouts
(defun layout-kanban()
  "Position current buffer in kanban layout."
  (interactive)
  (progn
    (setq mode-line-misc-info "Kanban layout")
    (split-window-below)
    (split-window-right)
    (split-window-right)
    (other-window -1)
    (split-window-right)
    (split-window-right)
    (other-window 3)
    (balance-windows)))

;; This causes the current time in the mode line to be displayed in
(use-package time
  :ensure t
  :config
  (progn
    (setq display-time-string-forms
          '((propertize (concat 24-hours ":" minutes))))
    (display-time)))
(use-package so-long
  :ensure t
  :config (global-so-long-mode))

;; something crashed in the upstream
(defun v-build-tags())

;; append name to org-mode's source block
(use-package org-tempo
  :ensure nil  ;; bundled in Emacs
  :after org)
(defun patch-org-insert-structure-template(type)
  "Patched 'org-insert-structure-template' to prefix with #+name: ?"
  (when (string-match-p "src" type)
    (insert (format "#+name: %s" (read-string "Block name ")))))
(defun patch-org-tempo-complete-tag(&rest _)
  "Patched 'org-tempo-complete-tag' to prefix with #+name: ?"
  (when (string-match-p "#\\+begin_src" (thing-at-point 'line t))
    (progn
      (org-beginning-of-line)
      (insert (format "#+name: %s\n" (read-string "Block name ")))
      (org-end-of-line)
      (org-delete-char 1)
      (insert " :results output :exports both")
      (backward-char 30)
      (insert (read-string "Language "))
      (org-end-of-line)
      (insert "\n"))))
(advice-add
 'org-insert-structure-template
 :before #'patch-org-insert-structure-template)
(advice-add
 'org-tempo-complete-tag
 :after #'patch-org-tempo-complete-tag)

;; macros
(defun uuid ()
  "Insert random-based UUID."
  (interactive)
  (and (insert (shell-command-to-string "echo -n $(uuidgen)")) ; await
       (forward-char 36)))
(defun date ()
  "Insert LC_TIME=C datetime."
  (interactive)
  (and (insert (shell-command-to-string "echo -n $(LC_TIME=C date)")) ; await
       (move-end-of-line 1)))
(defun youtube-feed-wrap-channel-id ()
  "Prefix YouTube channel ID (M-f jumpable string) with RSS URL."
  (interactive)
  (insert "https://www.youtube.com/feeds/videos.xml?channel_id=")
  (forward-word))
(defun youtube-video-link ()
  "Prefix YouTube video ID with full URL."
  (interactive)
  (let ((prefix "https://www.youtube.com/watch?v="))
    (if current-prefix-arg (insert prefix)
      (insert (format "%s%s" prefix (read-string "YouTube video ID: "))))))

(defun mpv-current ()
  "URL of the currently played video."
  (interactive)
  (message (cdr (assoc 'data (json-read-from-string
                              (shell-command-to-string "mpv-current"))))))
(defun mpv-links-in-playlist ()
  "URL of the currently played video."
  (interactive)
  (message (shell-command-to-string "mpv-playlist")))

;; multi-term common
;; assumptions: C-x 2|3 = (split-window-below|right)
;; hence: lowest ID = top-left, highest = bot-right
(defun window-id(win)
  "Return numeric window ID."
  (let ((win-name (format "%s" win))
        (pattern "^#<window \\([0-9]+\\)"))
    (string-match pattern win-name)
    (string-to-number (match-string 1 win-name))))

(defun window-list-sorted()
  "Sort (window-list) by their IDs (ascending)."
  (sort (window-list)
        (lambda (left right) (< (window-id left) (window-id right)))))

(defun go-to-first-window()
  "Navigate to the first open window (lowest ID) in frame."
  (interactive)
  (let ((continue t))
    (while continue
      (other-window 1)
      (when (eq (seq-position (window-list-sorted) (get-buffer-window)) 0)
        (setq continue nil)))))

(defun windows-to-buffers(wins)
  "Pull buffer refs from all windows in a frame."
  (let ((result ()))
    (dolist (item wins 'result)
      (push (window-buffer item) result))
    (nreverse result)))

(defun multi-term-leftside()
  "(multi-term) but spawn on left side of all windows."
  (interactive)
  (go-to-first-window)
  (let ((layouted-buffs (windows-to-buffers (window-list))))
    (delete-other-windows)
    (split-window-right)
    (multi-term)
    (other-window 1)
    (dolist (item layouted-buffs)
      (window--display-buffer
       item (get-buffer-window) 'reuse)
      (split-window-below)
      (other-window 1)
      (balance-windows))
    (delete-window)
    (other-window 1)
    (balance-windows)))

(defun rotate-windows(orientation)
  "Rotate windows layout horizontally/vertically."
  (let ((current-buff (buffer-name (current-buffer))))
    (go-to-first-window)
    (let ((buffs (windows-to-buffers (window-list))))
      (delete-other-windows)
      (dolist (item buffs)
        (window--display-buffer
         item (get-buffer-window) 'reuse)
        (if (string-equal orientation "h")
            (split-window-right)
          (split-window-below))
        (other-window 1)
        (balance-windows))
      (delete-window)
      (balance-windows)
      (while (not (eq current-buff (buffer-name (current-buffer))))
        (other-window 1)))))

(defun rotate-windows-horizontally()
  "Rotate windows layout horizontally."
  (interactive)
  (rotate-windows "h"))

(defun rotate-windows-vertically()
  "Rotate windows layout vertically."
  (interactive)
  (rotate-windows "v"))
;; end multi-term common

(defun my-org-roam-stop-and-clear ()
  "Stop Org Roam and clear everything unnecessary."
  ;; Stop the mode
  (org-roam-db-autosync-mode -1)

  ;; clear the DB and Emacs cached entries
  (org-roam-db-clear-all)
  (delete-file org-roam-db-location)
  (if (file-exists-p org-roam-db-location)
      (message "Failed to delete org-roam DB, delete manually!")
    (message "Successfully deleted org-roam DB"))
  (sleep-for 0.1))
(defun my-org-roam-start ()
  (unless (file-exists-p org-roam-directory)
    (make-directory org-roam-directory))
  (org-roam-db-autosync-mode))
(defun my-notes()
  "Start org-roam DB sync / load zettelkasten."
  (interactive)
  (my-org-roam-stop-and-clear)
  (my-set-org-roam-directory)
  (my-org-roam-start))

(defun org-roam-ext-find-by-tag (tag)
  (interactive "sTag: ")
  (let (opt-chars query selected options)
    (setq opt-chars
          (remove "" (split-string (concat "0123456789"
                                           "abcdefghijklmnopqrstuvwxyz"
                                           "ABCDEFGHIJKLMNOPQRSTUVWXYZ") "")))
    (setq query (format
                 "SELECT %s FROM %s %s WHERE tag like '%%%%%s%%%%'"
                 (string-join '("title" "file" "pos" "properties") ",")
                 "tags" "LEFT JOIN nodes n ON (n.id = node_id)" tag))
    (setq selected (org-roam-db-query query))

    (unless selected
      (error "No results from org-roam DB.  Are notes loaded?"))

    (dolist (item selected)
      (let (new)
        (setf (alist-get 'char new) (pop opt-chars)) ; unhandled out of chars
        (setf (alist-get 'title new) (car item))
        (setf (alist-get 'file new) (car (cdr item)))
        (setf (alist-get 'pos new) (car (cdr (cdr item))))
        (setf (alist-get 'tags new)
              (let ((all (car (cdr (cdr (cdr item))))) result)
                (dolist (single all)
                  (when (string= "ALLTAGS" (car single))
                    (setq result
                          (remove "" (split-string (cdr single) ":")))))
                result))
        (push new options)))
    (let (msg tmp)
      (dolist (opt (reverse options))
        (setq tmp (format "(%s) %s (%s)"
                          (alist-get 'char opt)
                          (alist-get 'title opt)
                          (string-join (alist-get 'tags opt) ", ")))
        (setq msg (if msg (format "%s\n%s" msg tmp) tmp)))
      (message msg))
    (let ((key (read-key)) result file-path)
      (dolist (opt options)
        (when (string= (string key) (alist-get 'char opt))
          (setq result opt)))
      (setq file-path (alist-get 'file result))
      (when file-path
        (find-file file-path)
        (goto-char (alist-get 'pos result))))))

;; Save desktop only when not screwed up while loading
(defvar my-desktop-save nil
  "Should I save the desktop when Emacs is shutting down?")
(add-hook 'desktop-after-read-hook
          (lambda () (setq my-desktop-save t)))
(advice-add
 'desktop-save :around
 (lambda (fn &rest args)
   (if (file-exists-p (expand-file-name (file-name-concat
                                         desktop-dirname
                                         desktop-base-file-name)))
       (when (bound-and-true-p my-desktop-save) (apply fn args))
     (apply fn args))))

(defun my-org-insert-todo-heading (fn &rest args)
  "Insert SCHEDULED and CREATED on new headings if in `todo.org'."
  (apply fn args)
  (when (and (string= (buffer-name (current-buffer)) "todo.org")
             (org-at-heading-p))
    (save-excursion
      (org-schedule nil "+0")
      (org-set-property "CREATED"
                        (with-temp-buffer
                          (let ((current-prefix-arg '(16)))
                            (call-interactively 'org-time-stamp-inactive))
                          (buffer-string))))))
(advice-remove 'org-insert-todo-heading 'my-org-insert-todo-heading)
(advice-add 'org-insert-todo-heading :around 'my-org-insert-todo-heading)

(defun my-org-roam-node-find (&rest _)
  "Ask for Roam title prefix so it's consistent a bit."
  (interactive)
  (if (not current-prefix-arg)
      (org-roam-node-find nil "")
    (let ((title "Choose my prefix")
          (tab (propertize " " 'display '(space :align-to 8)))
          tpl-map option buf-name initial-input)
      (save-window-excursion
        (save-mark-and-excursion
          (save-restriction
            (setq buf-name (make-temp-name (format "*%s*" title)))
            (switch-to-buffer-other-window buf-name)
            (insert (format "%s\n\n" title))

            (dolist (item my-org-roam-templates)
              (setf (alist-get (intern `,(car item)) tpl-map) (cdr item))
              (insert (concat (format "[%s]" (car item))
                              tab (cadr item) "\n")))

            (dotimes (_ (window-max-chars-per-line)) (insert "-"))

            (dolist (item '(("C" "Customize my-templates") ("q" "Abort")))
              (setf (alist-get (intern `,(car item)) tpl-map) (cdr item))
              (insert (concat (format "[%s]" (car item))
                              tab (cadr item) "\n")))

            (setq option (char-to-string (read-char "Prefix: ")))
            (kill-buffer buf-name)

            (setq initial-input
                  (cond ((string= option "C")
                         (user-error "No way to customize yet"))
                        ((string= option "q")
                         (message "Aborted")
                         "")
                        (t (setq option (alist-get (intern `,option) tpl-map))
                           (cadr option)))))))
      (org-roam-node-find nil initial-input))))

;; Popup for keybindings
(use-package which-key
  :ensure t
  :config (which-key-mode))
(use-package company
  :ensure t)
(use-package company-quickhelp
  :ensure t
  :after company)
(use-package dockerfile-mode
  :ensure t)
(use-package ace-window
  :ensure t
  :config
  (progn (global-set-key (kbd "M-o") #'ace-window)
         (add-hook 'term-mode-hook
                   (lambda () (keymap-set term-raw-map "M-o" #'ace-window)))))
(use-package httprepl
  :ensure t)

;; Stop the `list-processes' SIGKILL insanity
(defun terminate-process (proc)
  "Same as for `delete-process' + PID."
  (let ((sig 15))
    (message "Signal %s to %s" sig proc)
    (signal-process proc sig)))
(add-hook 'process-menu-mode-hook
          (lambda ()
            (keymap-set
             process-menu-mode-map "d"
             (lambda ()
               (interactive)
               (unwind-protect
                   (progn
                     (advice-add 'delete-process :override #'terminate-process)
                     (process-menu-delete-process))
                 (advice-remove 'delete-process #'terminate-process))))
            (keymap-set process-menu-mode-map "D"
                        #'process-menu-delete-process)))
;;; .emacs ends here
