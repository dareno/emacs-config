;;; init.el --- Initialization file for Emacs
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs
;; inspired by https://github.com/bbatsov/emacs.d/blob/master/init.el

;;; Code:
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

;; store all backup and autosave files in the tmp dir but omit tramp files
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(add-to-list 'backup-directory-alist
             (cons tramp-file-name-regexp nil)) ;; doesn't seem to work :(
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; replace buffer-menu with ibuffer
;; (global-set-key (kbd "C-x C-b") #'ibuffer)

;; Start proced in a similar manner to dired
;; conflicts with consult, disabling since I don't use it
;;(global-set-key (kbd "C-x p") #'proced)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; enable some commands that are disabled by default
(put 'erase-buffer 'disabled nil)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; remove with emacs 29 which will fix a bug
;; https://emacs.stackexchange.com/questions/74289/emacs-28-2-error-in-macos-ventura-image-type-invalid-image-type-svg
(add-to-list 'image-types 'svg)

(require 'use-package)
(setq use-package-verbose t)
(setq use-package-always-ensure t)

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
  :ensure f
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
  :ensure f
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

;; Helpful is an alternative to the built-in Emacs help that provides
;; much more contextual information.
(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  )

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package diminish
  :ensure t
  :config
  (diminish 'abbrev-mode)
  (diminish 'flyspell-mode)
  (diminish 'flyspell-prog-mode)
  (diminish 'eldoc-mode)
  (diminish 'visual-line-mode))

;; Enable Docker management and editing container files
(use-package docker ;; questionable use, replaces docker from CLI
  :ensure t
  :commands docker
  :bind ("C-c d" . docker))
(use-package docker-tramp  ;; need this to edit container files
  :defer t
  :after docker)

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)
    (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))))

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))
(set-face-attribute 'default nil :height 200)


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

;; todo, stop overlap with M-r cursor reposition
(use-package smartparens
  :ensure t
  :config
  (setq sp-show-pair-from-inside nil)
  (setq sp-base-key-bindings 'paredit)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings)
  (require 'smartparens-config)
  :diminish smartparens-mode
  :hook ('prog-mode . smartparens-mode)
  )

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
  :hook ('prog-mode . rainbow-delimiters-mode))

(set (make-local-variable 'whitespace-line-column) 80)
(add-hook 'after-change-major-mode-hook
          #'(lambda () (when (eq major-mode 'go-mode)
                        (setq whitespace-line-column 120))))
(add-hook 'after-change-major-mode-hook
          #'(lambda () (when (eq major-mode 'lsp-mode)
                        (setq whitespace-line-column 120))))

(use-package whitespace
  :init
  (diminish 'whitespace-mode )
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)

  :config
  ;; can't seem to turn tabs off for go-mode, maybe try to turn-off for lsp-mode?
  ;; (setq whitespace-style '(face tabs empty trailing lines-tail))
  (setq whitespace-style '(face empty trailing ))
  )

(use-package yaml-mode
  :ensure t)

;; Enable vertico
(use-package vertico
  :ensure t
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )
;; Optionally use the `orderless' completion style. See
;; `+orderless-dispatch' in the Consult wiki for an advanced Orderless style
;; dispatcher. Additionally enable `partial-completion' for file path
;; expansion. `partial-completion' is important for wildcard support.
;; Multiple files can be opened at once with `find-file' if you enter a
;; wildcard. You may also give the `initials' completion style a try.
(use-package orderless
  :ensure t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Example configuration for Consult
(use-package consult
  :ensure t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
)

;; (use-package selectrum-prescient
;;   :ensure t
;;   :config
;;   (selectrum-prescient-mode +1)
;;   (prescient-persist-mode +1)
;;   (selectrum-mode +1))

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

(use-package embark
  :ensure t

  :bind*
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package ctrlf
  :ensure t
  :config
  (ctrlf-mode +1))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.5)
  (setq company-show-quick-access t)
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
  :ensure f
  :config
  (setq org-agenda-directory "~/Dropbox/org/")
;;  (setq org-agenda-files '("~/Dropbox/org/" "~/Dropbox/org/habit/"))
  (setq org-agenda-files '("~/Dropbox/org/" ))
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
  :ensure f
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

;; (use-package org-habit
;;   :ensure nil
;;   :after org
;;   :config
;;   (setq org-habit-show-all-today t
;;         org-habit-preceding-days 7
;;         org-habit-following-days 1))

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
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

;; from https://legends2k.github.io/note/go_setup/
(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :config
  (defun my/go-mode-setup ()
    "Basic Go mode setup."
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t)
  ;; (setq whitespace-style '(face empty trailing )) ; same as global
  (setq tab-width 4)
  (setq indent-tabs-mode 1))
  (add-hook 'go-mode-hook #'my/go-mode-setup))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-mode lsp-deferred)
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         ((clojure-mode clojurec-mode go-mode js-mode) . lsp-deferred)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq lsp-prefer-flymake nil
        lsp-enable-indentation nil
        lsp-enable-on-type-formatting nil
        lsp-modeline-code-actions-enable t)
  (setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-minimum-prefix-length 1
      lsp-lens-enable t
      lsp-signature-auto-activate nil
      ; lsp-enable-indentation nil ; uncomment to use cider indentation instead of lsp
      ; lsp-enable-completion-at-point nil ; uncomment to use cider completion instead of lsp
      )
  ;; for filling args placeholders upon function completion candidate selection
  ;; lsp-enable-snippet and company-lsp-enable-snippet should be nil with
  ;; yas-minor-mode is enabled: https://emacs.stackexchange.com/q/53104
  ;; (lsp-modeline-code-actions-mode)
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (add-to-list 'lsp-file-watch-ignored "\\.vscode\\'")
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure"))))

;; optionally
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package treemacs
  :ensure t)

(use-package lsp-treemacs
  :ensure t
  :after lsp)

;; setup javascript/node.js environment
;; install nvm (git clone)
;; nvm install node
;; npm -i -g typescript-language-server
;; npm -i -g typescript
;; M-x run-js
(use-package js-comint
  :ensure t
  :config
  (js-do-use-nvm)
  (define-key js-mode-map [remap eval-last-sexp] #'js-comint-send-last-sexp)
  (define-key js-mode-map (kbd "C-c b") 'js-send-buffer)
;;  (define-key js-mode-map (kbd "C-M-b") 'js-send-buffer-and-go)
;;  (define-key js-mode-map (kbd "C-c b") 'js-send-buffer)

;;(with-eval-after-load 'js
;;  (define-key js-mode-map (kbd "M-.") nil))
)

(use-package cider
  :ensure t
  :config
  (setq nrepl-log-messages t)
  ;; (add-hook 'cider-repl-mode-hook #'paredit-mode)
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

;; RSS
(use-package elfeed
  :ensure t
  :config
  (setq elfeed-feeds
        '(
          "http://nullprogram.com/feed/"
          "https://planet.emacslife.com/atom.xml"
          "https://fs.blog/feed" "https://forum.prusaprinters.org/forum/hardware-firmware-and-software-help/?type=rss2&forum=384&topic=0"
         "https://forum.prusaprinters.org/forum/general-discussion-announcements-and-releases/?type=rss2&forum=381&topic=0"
         "http://rss.slashdot.org/Slashdot/slashdotMain"
         ))
  (setq-default elfeed-search-filter "@2-days-ago +unread")
  (setq elfeed-search-title-max-width 100)
  :bind
  ("C-x w" . elfeed ))

;; set latitude and longitude for noaa.el
(setq calendar-latitude 39.9)
(setq calendar-longitude -75.6)

;;   https://forecast.weather.gov/MapClick.php?w0=t&w1=td&w2=wc&w3=sfcwind&w4=sky&w5=pop&w6=rh&AheadHour=0&Submit=Submit&&FcstType=graphical&textField1=39.9065&textField2=-75.6008&site=all&menu=1

;;; general
(server-start)  ;; enable "ec" from the command line

;;; init.el ends here
