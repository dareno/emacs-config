;; inspired by https://github.com/bbatsov/emacs.d/blob/master/init.el

;; setup package manager
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; deal with a bug
;; https://emacs.stackexchange.com/questions/68288/error-retrieving-https-elpa-gnu-org-packages-archive-contents
(when (and (equal emacs-version "27.2")
           (eql system-type 'darwin))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(package-initialize)
;; update the package metadata is the local cache is missing
(unless package-archive-contents
  (package-refresh-contents))

(setq user-full-name "David Reno"
      user-mail-address "dcreno@gmail.com")

;; Always load newest byte code
(setq load-prefer-newer t)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; quit Emacs directly even if there are running processes
(setq confirm-kill-processes nil)

(defconst dcr-savefile-dir (expand-file-name "savefile" user-emacs-directory))
;; create the savefile dir if it doesn't exist
(unless (file-exists-p dcr-savefile-dir)
  (make-directory dcr-savefile-dir))

;; the toolbar is just a waste of valuable screen estate
;; in a tty tool-bar-mode does not properly auto-load, and is
;; already disabled anyway
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; disable startup screen
(setq inhibit-startup-screen t)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; word wrap
(global-visual-line-mode t)

;; mode line settings
(column-number-mode t)
(size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset: use that to adjust your
;; personal indentation width, while maintaining the style (and
;; meaning) of any files you load.
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent

;; Wrap lines at 80 characters
(setq-default fill-column 80)

;; I like line numbers
(global-display-line-numbers-mode)

;; delete the selection with a keypress
(delete-selection-mode t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; Start proced in a similar manner to dired
(global-set-key (kbd "C-x p") #'proced)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; enable some commands that are disabled by default
(put 'erase-buffer 'disabled nil)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(volatile-highlights ace-window which-key crux super-save company selectrum-prescient yaml-mode rainbow-mode rainbow-delimiters move-text exec-path-from-shell smartparens paredit diminish projectile git-timemachine magit zenburn-theme use-package)))

;;; built-in packages

;; highlight matching delimiters
(use-package paren
  :config
  (show-paren-mode +1))

;; insert matching delimiters (trying smart-parens instead)
(use-package elec-pair
  :config
  (electric-pair-mode +1))

;; highlight the current line
(use-package hl-line
  :config
  (global-hl-line-mode +1))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

;; saveplace remembers your location in a file when saving files
(setq save-place-file (expand-file-name "saveplace" dcr-savefile-dir))
(save-place-mode 1)

(use-package savehist
  :config
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file (expand-file-name "savehist" dcr-savefile-dir))
  (savehist-mode +1))

(use-package recentf
  :bind
  (("C-x C-r" . recentf-open-files))
  :config
  (setq recentf-save-file (expand-file-name "recentf" dcr-savefile-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (recentf-mode +1))

(use-package dired
  :config
  ;; dired - reuse current buffer by pressing 'a'
  (put 'dired-find-alternate-file 'disabled nil)

  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  ;; if there is a dired buffer displayed in the next window, use its
  ;; current subdir, instead of the current subdir of this dired buffer
  (setq dired-dwim-target t)

  ;; enable some really cool extensions like C-x C-j(dired-jump)
  (require 'dired-x))

;;; third-party packages
(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))
(set-face-attribute 'default nil :height 200)

(use-package diminish
  :ensure t
  :config
  (diminish 'abbrev-mode)
  (diminish 'flyspell-mode)
  (diminish 'flyspell-prog-mode)
  (diminish 'eldoc-mode))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package git-timemachine
  :ensure t
  :bind (("s-g" . git-timemachine)))

(use-package projectile
  :diminish projectile-mode
  :ensure t
  :init
  (setq projectile-project-search-path '("~/Documents/dev/"))
  :config
  ;; I typically use this keymap prefix on macOS
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  ;; On Linux, however, I usually go with another one
  (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
  (global-set-key (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(use-package smartparens
  :ensure t
  :config
  (setq sp-show-pair-from-inside nil)
  (setq sp-base-key-bindings 'paredit)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings)
  (require 'smartparens-config)
  :diminish smartparens-mode)

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package move-text
  :ensure t
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing lines-tail)))

(use-package yaml-mode
  :ensure t)

(use-package selectrum-prescient
  :ensure t
  :config
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1)
  (selectrum-mode +1))

;; Enable richer annotations using the Marginalia package
;; Mostly, I want keybinding hints with M-x minibuffer
(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :ensure t
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package ctrlf
  :ensure t
  :config
  (ctrlf-mode +1))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.5)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode)
  (diminish 'company-mode))

(use-package flyspell
  :config
  (when (eq system-type 'windows-nt)
    (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/"))
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra"))
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode))

(use-package super-save
  :ensure t
  :config
  ;; add integration with ace-window
  (add-to-list 'super-save-triggers 'ace-window)
  (super-save-mode +1)
  (diminish 'super-save-mode))

(use-package crux
  :ensure t
  :bind (("C-c o" . crux-open-with)
         ("M-o" . crux-smart-open-line)
         ("C-c n" . crux-cleanup-buffer-or-region)
         ("C-c f" . crux-recentf-find-file)
         ("C-M-z" . crux-indent-defun)
         ("C-c u" . crux-view-url)
         ("C-c e" . crux-eval-and-replace)
         ("C-c w" . crux-swap-windows)
         ("C-c D" . crux-delete-file-and-buffer)
         ("C-c r" . crux-rename-buffer-and-file)
         ("C-c t" . crux-visit-term-buffer)
         ("C-c k" . crux-kill-other-buffers)
         ("C-c TAB" . crux-indent-rigidly-and-copy-to-clipboard)
         ("C-c I" . crux-find-user-init-file)
         ("C-c S" . crux-find-shell-init-file)
         ("s-r" . crux-recentf-find-file)
         ("s-j" . crux-top-join-line)
         ("C-^" . crux-top-join-line)
         ("s-k" . crux-kill-whole-line)
         ("C-<backspace>" . crux-kill-line-backwards)
         ("s-o" . crux-smart-open-line-above)
         ([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ([(shift return)] . crux-smart-open-line)
         ([(control shift return)] . crux-smart-open-line-above)
         ([remap kill-whole-line] . crux-kill-whole-line)
         ("C-c s" . crux-ispell-word-then-abbrev)))

(use-package which-key
  :ensure t
  :config
  (which-key-mode +1)
  (diminish 'which-key-mode))

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "s-w") 'ace-window)
  (global-set-key [remap other-window] 'ace-window))

;; temporarily highlight changes from yanking, etc
(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode +1)
  (diminish 'volatile-highlights-mode))

;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

;;; Org Mode
(use-package org
  :config
  (setq org-directory "~/Dropbox/org")
  (setq org-default-notes-file (concat org-directory "/inbox.org"))
  (setq org-enforce-todo-dependencies t)
  (setq org-log-done (quote time))
  (setq org-log-redeadline (quote time))
  (setq org-log-reschedule (quote time))
  (setq org-todo-keywords
        (quote ((sequence "TODO(t)"
                          "NEXT(n)"
                          "|"
                          "DONE(d)")
                (sequence "WAITING(w@/!)"
                          "HOLD(h@/!)"
                          "|"
                          "CANCELLED(c@/!)"
                          "PHONE"
                          "MEETING"))))
  (setq org-use-fast-todo-selection t)
  (setq org-treat-S-cursor-todo-selection-as-state-change nil))

(use-package org-agenda
  :config
  (setq org-agenda-directory "~/Dropbox/org/")
  (setq org-agenda-files '("~/Dropbox/org/" "~/Dropbox/org/habit/"))
  (setq dcreno/org-agenda-todo-view
        `("a" "Agenda"
          ((agenda ""
                   ((org-agenda-span 'day)
                    (org-deadline-warning-days 365)))
           (todo "TODO"
                 ((org-agenda-overriding-header "To Refile")
                  (org-agenda-files
                   '(,(concat org-agenda-directory "inbox.org")))
                  ))
           (todo "NEXT"
                 ((org-agenda-overriding-header "In Progress")
                  ))
           (todo "TODO"
                 ((org-agenda-overriding-header "Someday - work")
                  (org-agenda-files
                   '(,(concat org-agenda-directory "comcast.org")))
                  (org-agenda-skip-function
                   '(org-agenda-skip-entry-if 'scheduled))
                  ))
           (todo "TODO"
                 ((org-agenda-overriding-header "Someday - personal")
                  (org-agenda-files
                   '(,(concat org-agenda-directory "personal.org")))
                  (org-agenda-skip-function
                   '(org-agenda-skip-entry-if 'scheduled))
                  ))
           nil)))
  (set 'org-agenda-custom-commands `(,dcreno/org-agenda-todo-view)))

(use-package org-faces
  :config
  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "red" :weight bold)
                ("NEXT" :foreground "blue" :weight bold)
                ("DONE" :foreground "forest green" :weight bold)
                ("WAITING" :foreground "orange" :weight bold)
                ("HOLD" :foreground "magenta" :weight bold)
                ("CANCELLED" :foreground "forest green" :weight bold)
                ("MEETING" :foreground "forest green" :weight bold)
                ("PHONE" :foreground "forest green" :weight bold)))))

(use-package org-journal
  :ensure t
  :after org
  :custom
  (org-journal-dir (concat org-directory "/journal")))

;;(make-directory "~/Dropbox/org-roam")

(use-package org-roam
  :ensure t
  :preface
  (setq org-roam-v2-ack t)  ;; load first to avoid migration warning
  :after org
  :diminish
  :custom
  (setq org-roam-directory "~/Dropbox/org-roam")
  ;; index-file doesn't seem to be a variable anymore
  ;; (org-roam-index-file "~/Dropbox/org-roam/20210114150306-index.org")
  ;; (add-hook 'after-init-hook 'org-roam-mode) ;; error after update
  )

(use-package deft
  :bind ("<f9>" . deft)
  :after org
  :commands (deft)
  :config (setq deft-directory "~/Dropbox/org"
                deft-extensions '("org")))

(use-package org-habit
  :ensure nil
  :after org
  :config
  (setq org-habit-show-all-today t
        org-habit-preceding-days 7
        org-habit-following-days 1))

(use-package org-capture
  :ensure nil
  :after org
  :config
  (setq org-capture-templates
        `(("i" "inbox" entry
           (file ,(concat org-agenda-directory "inbox.org"))
           "* TODO %?")
          ("w" "work" entry
           (file ,(concat org-agenda-directory "comcast.org"))
           "* TODO %?")
          ("p" "personal" entry
           (file ,(concat org-agenda-directory "personal.org"))
           "* TODO %?"))))

;; for refiling org-agenda tasks from inbox.org
(use-package org-refile
  :ensure nil
  :after org
  :config
  (setq org-refile-targets
        (quote (("comcast.org" :maxlevel . 1)
                ("personal.org" :maxlevel . 1)))))

;;; Programming
(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (clojure-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package cider
  :ensure t
  :config
  (setq nrepl-log-messages t)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode))

;; hide-show mode
(use-package hideshow
  :init
  (add-hook #'prog-mode-hook #'hs-minor-mode)
  (add-hook #'emacs-lisp-mode-hook #'hs-minor-mode)
  :diminish hs-minor-mode
  :config
  (define-key hs-minor-mode-map (kbd "C-c h")
    (lookup-key hs-minor-mode-map (kbd "C-c @"))))

;;; init.el ends here
