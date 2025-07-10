;;; .emacs --- Emacs config -*- lexical-binding: t; -*-

;; Copyright (C) 2016 - 2025, KeyWeeUsr(Peter Badida) <keyweeusr@gmail.com>

;; Author: KeyWeeUsr
;; Version: 5.7

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

;; todo(compile): with-suppressed-warnings vs with-no-warnings

(let ((path (expand-file-name "early-init.el" user-emacs-directory)))
  (unless (file-exists-p path)
    (with-temp-file path
      (insert ";; Example Elpaca early-init.el -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
"))
    (unless (boundp 'org-roam-db-location)
      (with-no-warnings
        ;; note(free-var,defvar-shadow): org-roam-db before needed
        (setq org-roam-db-location "/tmp/ignore-me")))
    (restart-emacs)
    (error "https://www.youtube.com/watch?v=OCsMKypvmB0")))

;; note(elpaca,begin): installer
(defvar elpaca-lock-file (format "%s.elpaca.lock" user-init-file))
(defvar elpaca-installer-version 0.10)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order
  '(elpaca :repo "https://github.com/progfolio/elpaca.git"
           :ref nil :depth 1 :inherit ignore
           :files (:defaults "elpaca-test.el" (:exclude "extensions"))
           :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop
                    (apply #'call-process
                           `("git" nil ,buffer t "clone"
                             ,@(when-let* ((depth (plist-get order :depth)))
                                 (list (format "--depth=%d" depth)
                                       "--no-single-branch"))
                             ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process
                           emacs nil buffer nil "-Q" "-L" "." "--batch"
                           "--eval"
                           "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((with-no-warnings
                     ;; note(circular): compile requires elpaca, elpaca needs to bootstrap
                     (elpaca-generate-autoloads "elpaca" repo))))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (with-no-warnings
      ;; note(circular): compile requires elpaca, elpaca needs to bootstrap
      (require 'elpaca))
    (with-no-warnings
      ;; note(circular): compile requires elpaca, elpaca needs to bootstrap
      (elpaca-generate-autoloads "elpaca" repo))
    (load "./elpaca-autoloads")))
(with-no-warnings
  ;; note(circular): compile requires elpaca, elpaca needs to bootstrap
  (add-hook 'after-init-hook #'elpaca-process-queues)
  (elpaca `(,@elpaca-order)))
;; note(elpaca,end): installer

;; Enable use-package :ensure support for Elpaca.
(with-no-warnings
  ;; note(free-var): unquoted symbol of an extension in a macro false-positive
  (elpaca elpaca-use-package
    (elpaca-use-package-mode)))

;; constants, etc
(defvar my-epa-file-encrypt-to "")

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
   '(company elfeed-tube elfeed mermaid-mode org-roam langdoc ace-window ascii-art-to-unicode auto-complete brainfuck-mode cmake-mode company-quickhelp curl-to-elisp cython-mode dbml-mode decor dedicated define-it digit-groups diminish dired-duplicates docker-compose-mode dockerfile-mode ecukes elfeed-org elfeed-tube-mpv ess exec-path-from-shell gnu-elpa-keyring-update go-mode gradle-mode graphviz-dot-mode groovy-mode helpful htmlize httprepl keepass-mode kivy-mode kotlin-mode markdown-mode mermaid-docker-mode multi-term ob-base64 org-epa-gpg org-roam-timestamps org-roam-ui org-transclusion org-web-tools osm package-lint php-mode ssh-config-mode terraform-mode typewriter-roll-mode undercover use-package v-mode wc-mode which-key yaml-mode))
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

(defun my-move (left-top)
  "Move window to comma-separated LEFT-TOP location."
  (interactive "sLeft,Top: ")
  (let* ((left (string-to-number
                (car (split-string left-top "," nil))))
         (top (string-to-number
               (cadr (split-string left-top "," nil)))))
    (modify-frame-parameters (window-frame)
                             `((left . ,left) (top . ,top)))))
(defun my-move-left () (interactive) (my-move "-2000,0"))
(defun my-move-right () (interactive) (my-move "2000,0"))
(defun my-move-top () (interactive) (my-move "0,1000"))
(defun my-move-bottom () (interactive) (my-move "0,-1000"))
(defun my-move-middle () (interactive) (my-move "-2000,0"))
(defun my-move-center () (interactive) (my-move "-2000,0"))

;; note(macos,begin): fixes & patching around
(progn
;; Fixes for MacOS
(when (eq window-system 'ns)
  (with-no-warnings
    (setq ns-right-alternative-modifier 'none)
    (setq mac-right-option-modifier 'none)
    (setq ns-command-modifier 'meta)
    (setq mac-command-modifier 'meta))
  (advice-add 'ns-print-buffer :override (lambda (&rest _))
              '((name . "mac-keyboard")))
  (setq exec-path (append '("/opt/homebrew/bin") exec-path)))

(use-package tool-bar
  :ensure nil
  :init
  (defun my-fix-mac ()
    "Fix Emacs breaking on MacOS."
    (interactive)
    (eval-when-compile (require 'tool-bar))
    (when (eq window-system 'ns)
      (tool-bar-mode -1)
      (setq frame-resize-pixelwise t)
      (dotimes (_ 3)
        (toggle-frame-maximized)))))
)
;; note(macos,end): fixes & patching around

(defun custom-exit ()
  "Run at exit."
  (interactive "P")
  (require 'org-roam-db)
  (progn
    (progn
      (message "Deleting MPV playlist")
      (delete-file "/tmp/mpvplaylist")
      t)
    (progn
      (message "Deleting org-roam DB")
      (if (not (boundp 'org-roam-db-location))
          (progn (warn "Missing org-roam-db-location")
                 (when current-prefix-arg t))
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
              t)))))))
(unless (string= window-system "android")
  (add-hook 'kill-emacs-query-functions 'custom-exit))

;; note(cve,begin): exploits and other fixes
;; 1) LaTeX & Org exploit fix
(when (and (< emacs-major-version 29)
           (< emacs-minor-version 3))
  (with-no-warnings
    ;; note(free-var): backward compatible to older versions
    (setq org-preview-latex-default-process 'verbatim)))
;; note(cve,end): exploits and other fixes

;; note(keepass,begin): KeePass config
(progn
(declare-function my-cache-gpg-key ".emacs")
(defvar my-cache-gpg-key-timer)

(defun my-cache-gpg-progress
    (context operation display-chr current total data)
  "From epg-context-set-progress-callback."
  (ignore context operation display-chr data)
  (when (= current total)
    (run-at-time
     2 nil (lambda (&rest _)
             (setq kill-ring (cdr kill-ring))
             (when (display-graphic-p) (gui-select-text "ok"))
             (message "Clipboard cleared")))))

(defun my-string-or (what default)
  (if (string= "" what) default what))

(defvar my-imgur-client-id)
(defvar my-imgur-client-secret)
(defvar my-org-roam-directory)
(defvar my-org-roam-templates)
(defvar my-epa-file-encrypt-to)
(defvar my-gpg-id)

(declare-function cred ".emacs")
(use-package epg
  :ensure nil
  :init
  (progn
    (defun my-cache-gpg-key (&optional keep ctx)
      (interactive)
      (eval-when-compile (require 'epg))
      (unless ctx (setq ctx (epg-make-context)))
      (setf (epg-context-pinentry-mode ctx) 'loopback)

      (if (with-no-warnings
            ;; todo(undefined,declare-shadow): has source, ignored require
            (epg-sign-string ctx "\n"))
          (message "Pre-loaded GPG key")
        (warn "Pre-loading GPG key failed"))

      (when keep (setq my-cache-gpg-key-timer
                       (run-with-timer 1 290 #'my-cache-gpg-key))))
    (defun my-cache-gpg-key-cancel ()
      (interactive)
      (cancel-function-timers #'my-cache-gpg-key))
    (defun my-keepass-init ()
      (interactive)
      (eval-when-compile (require 'epg))
      (cancel-function-timers #'my-cache-gpg-key)
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
        (ignore keepass-mode-password)
        (fset 'cred `(lambda (what type)
                       (keepass-mode-get
                        type (format "emacs-creds/%s-%s" what ,user))))
        (require 'subr-x)
        (with-no-warnings
          ;; note(free-var,defvar-shadow): syncthing before needed
          (setq syncthing-default-server-token
                (when keepass-mode-db
                  (cred "syncthing-token" const-password))))
        (setq my-imgur-client-id
              (when keepass-mode-db (cred "imgur-api" const-username)))
        (setq my-imgur-client-secret
              (when keepass-mode-db (cred "imgur-api" const-password)))
        ;; Only unless found set to default, otherwise nil
        (with-no-warnings
          ;; note(free-var,defvar-shadow): elfeed before needed
          (setq elfeed-db-directory
                (when keepass-mode-db
                  (my-string-or
                   (cred "elfeed-db-dir" const-username) "~/elfeed"))))
        ;; Only unless found set to default, otherwise nil
        (setq my-org-roam-directory
              (with-no-warnings
                ;; note(free-var,defvar-shadow): keepass-mode before needed
                (when keepass-mode-db
                  (my-string-or (cred "org-roam-dir" const-username)
                                (expand-file-name
                                 "roam" user-emacs-directory)))))
        (setq my-org-roam-templates
              (with-no-warnings
                ;; note(free-var,defvar-shadow): keepass-mode before needed
                (when keepass-mode-db
                  (mapcar (lambda (item) (split-string item ";"))
                          (split-string
                           (cred "org-roam-templates" const-notes) ";;")))))
        (setq my-epa-file-encrypt-to
              (when keepass-mode-db
                (cred "epa-file-encrypt-to" const-username)))
        (setq my-gpg-id (when keepass-mode-db (cred "gpg-main" const-notes)))
        (let ((tmp nil) (epg-user-id my-gpg-id) (ctx (epg-make-context))
              (select-enable-clipboard t) (select-enable-primary t))
          (with-no-warnings
            ;; todo(undefined,declare-shadow): has source, ignored require
            (epg-context-set-progress-callback ctx #'my-cache-gpg-progress))
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
        (warn "KeePass init failed")))))

(use-package keepass-mode
  :ensure (:depth 1)
  :after epg
  :config
  (if (not (eq window-system 'android))
      (add-hook 'elpaca-after-init-hook #'my-keepass-init)
    (let ((vars '(elfeed-db-directory
                  syncthing-default-server-token)))
      (dolist (var vars)
        (let* ((tmpl "Setting var '%s' [%S] ")
               (current (when (boundp var) (symbol-value var)))
               (new (read-string (format tmpl var current))))
          (unless (string= new "")
            (setf (symbol-value var) new)))))))
)
;; note(keepass,end): KeePass config

(use-package auth-source
  :after keepass-mode
  :ensure nil
  :config
  (unless (string= window-system "android")
    (setq auth-sources '("~/.authinfo.gpg"))
    (setq auth-source-gpg-encrypt-to (list my-epa-file-encrypt-to))))

(use-package syncthing
  :ensure (:depth 1)
  :config
  (progn
    (setq syncthing-header-items
          '("rate-download" "rate-upload" "count-local-files"
            "count-local-folders" "count-local-bytes" "count-listeners"
            "count-discovery" "uptime" "my-id" "version"))))

;; todo: possibly unnecessary
(use-package ansi-color
  :ensure nil
  :config
  (progn
    (let ((what
           [default default default italic underline success warning error]))
      (with-no-warnings
        ;; obsoleted 28.1+
        (setq ansi-color-faces-vector what))
      (setq ansi-color-normal-colors-vector what)))
  :init
  ;; note(multi-term,color): render ansi escape colors
  ;; note: possibly https://stackoverflow.com/a/23382008/5994041
  (defun ansi-color-region()
    "Interactive version of func."
    (interactive)
    (eval-when-compile (require 'ansi-color))
    (with-no-warnings
      ;; todo(undefined,declare-shadow): has source, ignored require
      (ansi-color-apply-on-region (point-min) (point-max)))))

(use-package cc-vars
  :ensure nil
  :config
  (progn
    ;; todo(bad-value): docstring doesn't seem to support "k&r"
    (setq c-basic-offset 4)
    (setq c-default-style "k&r")
    (setq c-offsets-alist '((arglist-close . +)))))
(use-package cc-mode
  :ensure nil
  :after cc-vars
  :config
  (progn
    (with-no-warnings
      ;; todo(free-var,defvar-shadow,deprecated): no source, does not exist
      (setq c-basic-indent 4))
    (with-no-warnings
      ;; todo(free-var,defvar-shadow,deprecated): no source, does not exist
      (setq c-set-offset 'arglist-close))))

(use-package simple
  :ensure nil  ;; bundled in Emacs
  :config (column-number-mode t))

(use-package delsel
  :ensure nil
  :config (delete-selection-mode t))

(use-package auto-complete
  :ensure (:depth 1)
  :config
  (progn
    (ac-config-default)
    (setq ac-use-menu-map t)
    (advice-add 'create-file-buffer
                :after (lambda (&rest _) (acon))))
  :init
  (progn
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
      (interactive) (ac-onoff -1))))

(use-package desktop
  :ensure nil
  :config (desktop-save-mode t))

(use-package dired
  :ensure nil  ;; bundled in Emacs
  :config (setq dired-listing-switches "-al"))

(use-package fringe
  :ensure nil  ;; bundled in Emacs
  :config
  (progn (eval-when-compile (require 'fringe))
         (with-no-warnings
           ;; todo(undefined,declare-shadow): has source, ignored require
           (fringe-mode 8))))

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
  :ensure nil
  :config (setq ispell-dictionary nil))

(use-package js
  :ensure nil
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

(use-package langdoc :ensure (:depth 1))
(use-package ascii-art-to-unicode :ensure (:depth 1))
(use-package bind-key :ensure nil)
(use-package brainfuck-mode :ensure (:depth 1))
(use-package diminish :ensure (:depth 1))
(use-package popup :ensure (:depth 1))
(use-package php-mode :ensure (:depth 1))
(use-package gradle-mode :ensure (:depth 1))
(use-package ssh-config-mode :ensure (:depth 1))
(use-package v-mode :ensure (:depth 1))

(use-package term
  :ensure nil
  :config
  (progn
    (setq term-buffer-maximum-size 0))
  :init
  (progn
    ;; escape nano w/ C-x
    (defun term-send-C-x ()
      "Send `C-x' to terminal."
      (interactive)
      (eval-when-compile (require 'term))
      (with-no-warnings
        ;; todo(undefined,declare-shadow): has source, ignored require
        (term-send-raw-string "\C-x")))
    (defun my-open-pr ()
      "Follow a link from git remote to open a PR."
      (interactive)
      (save-excursion
        (unwind-protect
            (progn
              (with-no-warnings
                ;; todo(undefined,declare-shadow): has source, ignored require
                (term-mode))
              (re-search-backward "Create a pull request")
              (re-search-forward "http") (re-search-backward "http")
              (unwind-protect
                  (progn
                    (push-mark)
                    (let ((what "^remote:"))
                      (re-search-forward what) (re-search-backward what))
                    (backward-char 1)
                    (browse-url
                     (string-replace "\n" "" (buffer-substring-no-properties
                                              (mark) (point)))))
                (pop-mark)))
          (with-no-warnings
            ;; todo(undefined,declare-shadow): has source, ignored require
            (term-char-mode)))))))

(defun windows-to-buffers(wins)
  "Pull buffer refs from all windows in a frame."
  (let ((result ()))
    (dolist (item wins 'result)
      (push (window-buffer item) result))
    (nreverse result)))

(use-package multi-term
  :ensure (:depth 1)
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
            ("C-c C-j" . (lambda () (interactive) (term-mode)))
            ("C-c C-k" . (lambda () (interactive) (term-char-mode)))
            ("C-c C-o" . my-open-pr))))
  :init
  (progn
    (defun m()
      "(multi-term) shortcut."
      (interactive)
      (multi-term))
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
        (balance-windows)))))

(use-package wc-mode :ensure (:depth 1))

(use-package graphviz-dot-mode
  :ensure (:depth 1))

(use-package org
  :ensure (:depth 1 :wait t)
  :after (graphviz-dot-mode verb)
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
            (verb . t)
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
    (setq org-src-tab-acts-natively t))
  :init
  (progn
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
    (advice-add 'org-insert-todo-heading :around 'my-org-insert-todo-heading)))

;; note(org-roam,begin): zettelkasten
(progn
(declare-function my-set-org-roam-directory ".emacs")
(declare-function my-org-roam-node-find ".emacs")
(use-package org-roam
  :ensure (:depth 1)
  :after (keepass-mode org)
  :config
  (progn
    ;; (setq org-roam-database-connector 'libsqlite3)
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
  :init
  (progn
    (defun my-set-org-roam-directory ()
      "Set `org-roam-directory'."
      (interactive)
      (eval-when-compile (require 'org-roam))
      (setq org-roam-directory my-org-roam-directory))
    (defun my-org-roam-node-find (&rest _)
      "Ask for Roam title prefix so it's consistent a bit."
      (interactive)
      (eval-when-compile (require 'org-roam))
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
                            (t (setq option
                                     (alist-get (intern `,option) tpl-map))
                               (cadr option)))))))
          (org-roam-node-find nil initial-input)))))
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
(use-package org-roam-db
  :after org-roam
  :ensure (:depth 1)
  :init
  (defun org-roam-ext-find-by-tag (tag)
    (interactive "sTag: ")
    (let (opt-chars query selected options)
      (setq opt-chars
            ;; (number-sequence 1 5) -> [1] .. [5]
            ;; (string 48) .. (string 57)
            ;; (string 65) .. (string 90)
            ;; (string 97) .. (string 122)
            (mapcar (lambda (num) (string num))
                    ;; note: 0-9 + A-Z + a-z
                    (append (number-sequence 48 57)
                            (number-sequence 65 90)
                            (number-sequence 97 122))))
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
  (defun my-org-roam-stop-and-clear ()
    "Stop Org Roam and clear everything unnecessary."
    (require 'org-roam-db)
    (org-roam-db-autosync-mode -1)

    ;; clear the DB and Emacs cached entries
    (org-roam-db-clear-all)
    (when (boundp 'org-roam-db-location)
      (delete-file org-roam-db-location)
      (if (file-exists-p org-roam-db-location)
          (message "Failed to delete org-roam DB, delete manually!")
        (message "Successfully deleted org-roam DB"))
      (sleep-for 0.1)))
  (defun my-notes()
    "Start org-roam DB sync / load zettelkasten."
    (interactive)
    (my-org-roam-stop-and-clear)
    (my-set-org-roam-directory)
    (my-org-roam-start))
  (defun my-org-roam-start ()
    (when (boundp 'org-roam-directory)
      (unless (file-exists-p org-roam-directory)
        (make-directory org-roam-directory)))
    (org-roam-db-autosync-mode)))
)
;; note(org-roam,end): zettelkasten

(use-package org-roam-timestamps
  :ensure (:depth 1)
  :after org-roam
  :config
  (progn
    (setq org-roam-timestamps-minimum-gap 60)
    (org-roam-timestamps-mode)))

(use-package org-roam-ui
  :after org-roam
  :ensure (:depth 1)
  ;; https://github.com/org-roam/org-roam-ui/issues/202
  :init (progn (add-to-list 'desktop-minor-mode-table
                            '(org-roam-ui-mode nil))
               (add-to-list 'desktop-minor-mode-table
                            '(org-roam-ui-follow-mode nil))
               (setq org-roam-ui-open-on-start nil)))

(use-package decor
  :ensure (:depth 1)
  :config (progn (decor-mode)
                 (add-hook 'elpaca-after-init-hook 'decor-all-frames-off)))

(use-package mermaid-mode
  :ensure (:depth 1))

(use-package mermaid-docker-mode
  :ensure (:depth 1)
  :after mermaid-mode
  :config (progn (setq mermaid-docker-external-viewer-bin "/usr/bin/xviewer")
                 (setq mermaid-docker-focus-steal-ms 100)
                 (setq mermaid-docker-stay-in-window t)))

(use-package typewriter-roll-mode
  :ensure (:depth 1))


;; Separate plugins folder
(let ((plugins-dir (expand-file-name "plugins" user-emacs-directory)))
  (unless (file-exists-p plugins-dir) (make-directory plugins-dir))
  (add-to-list 'load-path plugins-dir))

;; Kivy .kv syntax plugin
(use-package kivy-mode
  :ensure (:depth 1))

(use-package iso-transl  ;; allow dead-acute + ibus for japanese
  :ensure nil)
;; bundled in Emacs

(use-package epa-file  ;; encryption for .gpg files
  :ensure nil  ;; bundled in Emacs
  :init (unless window-system (setf epg-pinentry-mode 'loopback))
  :config (epa-file-enable))

(use-package org-epa-gpg
  :ensure (:depth 1)
  :after epa-file)

(use-package ob-base64
  :ensure (:depth 1)
  :after org
  :config
  (setf (alist-get 'base64 org-babel-load-languages) t))

(use-package dbml-mode
  :ensure (:depth 1))

(use-package elfeed
  :ensure (:depth 1)
  :config
  (setq elfeed-search-filter "@2023-02-07T23:59 +unread ")
  :bind (:map elfeed-search-mode-map
              ("R" . elfeed-search-untag-all-unread---reversed)
              ("A" . elfeed-search--append-to-mpv-playlist---reversed)
              ("a" . elfeed-search--append-to-mpv-playlist)
              ("o" . elfeed-search--open-link))
  :bind (:map elfeed-show-mode-map
              ("C-c C-o" . elfeed-show-visit))
  :init
  (progn
    (defun elfeed-search-untag-all-unread---reversed ()
      "Reverse \\='r' behavior in elfeed."
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
      ;; (eval-when-compile (require 'elfeed))
      (elfeed-search-browse-url)
      (when current-prefix-arg
        (forward-line -1)
        ;; todo(elfeed): unalias
        (elfeed-search-tag-all-unread)))))

;; note(elfeed,org,begin): elfeed config from org file
(progn
(use-package shr
  :ensure nil
  :config (setq shr-inhibit-images t))

(use-package elfeed-org
  :ensure (:depth 1)
  :after (elfeed shr)
  :config
  (progn
    (setq rmh-elfeed-org-files (list (format "%s.org" elfeed-db-directory)))
    (elfeed-org)))
)
;; note(elfeed,org,end): elfeed config from org file

;; note(elfeed,youtube,begin): YouTube channels in Elfeed
(use-package json
  :ensure nil)
(use-package elfeed-tube
  :ensure (:depth 1)
  :after (json elfeed-org)
  :demand t
  :config
  ;; (setq elfeed-tube-auto-save-p nil) ; default value
  ;; (setq elfeed-tube-auto-fetch-p t)  ; default value
  (progn
    (setq elfeed-tube-fields '(duration description))
    (setq elfeed-tube-mpv-options
          '("--cache=yes" "--ytdl-format=22" "--save-position-on-quit"))
    (elfeed-tube-setup))
  :init
  (progn
    (defun mpv-current ()
      "URL of the currently played video."
      (interactive)
      (eval-when-compile (require 'json))
      (message (cdr (assoc 'data (json-read-from-string
                                  (shell-command-to-string "mpv-current"))))))
    (defun mpv-links-in-playlist ()
      "URL of the currently played video."
      (interactive)
      (message (shell-command-to-string "mpv-playlist"))))
  :bind (:map elfeed-show-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)
         :map elfeed-search-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)))
;; note(elfeed,youtube,end): YouTube channels in Elfeed

;; note(elfeed,youtube,mpv,begin): Local playing
(use-package elfeed-tube-mpv
  :ensure (:depth 1)
  :after elfeed-tube
  :bind (:map elfeed-show-mode-map
         ("C-c C-m" . elfeed-tube-mpv)
         ("C-c C-f" . elfeed-tube-mpv-follow-mode)
         ("C-c C-w" . elfeed-tube-mpv-where)))
;; note(elfeed,youtube,mpv,end): Local playing

(use-package terraform-mode
  :ensure (:depth 1))

;; note(helpful,begin): Better help pages
(use-package helpful
  :ensure (:depth 1)
  :config
  (progn
    (global-set-key (kbd "C-h f") #'helpful-callable)
    (global-set-key (kbd "C-h v") #'helpful-variable)
    (global-set-key (kbd "C-h k") #'helpful-key)
    (global-set-key (kbd "C-h x") #'helpful-command)))
;; note(helpful,end): Better help pages

;; note(window,begin): Bindings for quicker window size and buffer manipulation
(global-set-key (kbd "C-c <up>") 'shrink-window)
(global-set-key (kbd "C-c <down>") 'enlarge-window)
(global-set-key (kbd "C-c <left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-c <right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-c |") 'multi-term-leftside)
(global-set-key (kbd "<f12>") 'kill-buffer-and-window)
;; note(window,end): Bindings for quicker window size and buffer manipulation

;; note(unindent,begin): https://stackoverflow.com/a/2250155/5994041
;; Add <backtab> unindenting
;;(global-set-key (kbd "<backtab>") (read-kbd-macro "<backspace>"))
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
;; note(unindent,end): https://stackoverflow.com/a/2250155/5994041

;; note(hooks,begin): custom code for modes
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
;; note(hooks,end): custom code for modes

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
  :ensure nil
  :config
  (progn
    (setq display-time-string-forms
          '((propertize (concat 24-hours ":" minutes))))
    (display-time)))

(use-package so-long
  :ensure nil
  :config (global-so-long-mode))


;; note(vlang): something crashed in the upstream on buffer save
(defun v-build-tags())

(declare-function patch-org-insert-structure-template ".emacs")
(declare-function patch-org-tempo-complete-tag ".emacs")
(use-package org-tempo
  :ensure nil  ;; bundled in Emacs
  :after org
  :init
  (progn
    ;; note(org,begin): append name to org-mode's source block
    (defun patch-org-insert-structure-template (type)
      "Patched \\='org-insert-structure-template' to prefix with #+name: ?"
      (when (string-match-p "src" type)
        (insert (format "#+name: %s" (read-string "Block name ")))))
    (defun patch-org-tempo-complete-tag (&rest _)
      "Patched \\='org-tempo-complete-tag' to prefix with #+name: ?"
      (when (string-match-p "#\\+begin_src" (thing-at-point 'line t))
        (progn
          (beginning-of-line)
          (insert (format "#+name: %s\n" (read-string "Block name ")))
          (end-of-line)
          (delete-char 1)
          (insert " :results output :exports both")
          (backward-char 30)
          (insert (read-string "Language "))
          (end-of-line)
          (insert "\n"))))
    (advice-add
     'org-insert-structure-template
     :before #'patch-org-insert-structure-template)
    (advice-add
     'org-tempo-complete-tag
     :after #'patch-org-tempo-complete-tag)
    ;; note(org,end): append name to org-mode's source block
    ))

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

;; note(window,begin): navigation and selection
(progn
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
)
;; note(window,end): navigation and selection

;; note(window,begin): rotation
(progn
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
)
;; note(window,end): rotation

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

;; Popup for keybindings
(use-package which-key
  :ensure (:depth 1)
  :config (which-key-mode))

(use-package company
  :ensure (:depth 1))

(use-package company-quickhelp
  :ensure (:depth 1)
  :after company)

(use-package dockerfile-mode
  :ensure (:depth 1))

(use-package ace-window
  :ensure (:depth 1)
  :config
  (progn (global-set-key (kbd "M-o") #'ace-window)
         (add-hook 'term-mode-hook
                   (lambda () (keymap-set term-raw-map "M-o" #'ace-window)))))

(use-package httprepl
  :ensure (:depth 1))

(use-package go-mode
  :ensure (:depth 1))

(use-package yaml-mode
  :ensure (:depth 1))

(use-package markdown-mode
  :ensure (:depth 1))

(use-package lark-mode
  :ensure (:depth 1))

(use-package rust-mode
  :ensure (:depth 1))

(use-package feature-mode
  :ensure (:depth 1)
  :init
  ;; broken upstream recipe, copy the .json
  (let* ((langs-name "gherkin-languages.json")
         (recipe (elpaca-recipe 'feature-mode))
         (build-dir (elpaca-build-dir recipe))
         (langs (file-name-concat build-dir langs-name)))
    (unless (file-exists-p langs)
      (copy-file (file-name-concat (elpaca-repo-dir recipe) langs-name)
                 langs))))

(use-package verb
  :ensure (:depth 1))

(use-package package-lint
  :ensure (:depth 1))

(use-package ecukes
  :ensure (:depth 1))

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
