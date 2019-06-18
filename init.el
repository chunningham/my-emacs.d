;; Package configs

;;; Code:
(require 'package)
(setq package-enable-at-startup nil); tells emacs not to load any packages before starting up
;; the following lines tell emacs where on the internet to look up
;; for new packages.
(setq package-archives '(("org"	  . "http://orgmode.org/elpa/")
			 ("gnu"	  . "http://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))

(require 'use-package)

;; Minimal UI
(scroll-bar-mode   -1)
(tool-bar-mode     -1)
(tooltip-mode      -1)
(menu-bar-mode     -1)
(blink-cursor-mode  0)
(global-linum-mode  1)

;; Other configs
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq ring-bell-function 'ignore )	; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8 )	; use utf-8 by default
(setq coding-system-for-write 'utf-8 )
(setq delete-old-versions -1 )		; delete excess backup versions silently

;; Splash Screen
(setq inhibit-startup-screen t)
(setq initial-scratch-message ";; Happy Hacking")

;; Show matching parens
(setq show-paren-delay 0)
(show-paren-mode  1)

;; Evil
(use-package evil
  :ensure t
  :config
  (evil-mode 1))

;;(use-package xah-fly-keys
;;  :ensure t
;;  :config
;;  (xah-fly-keys-set-layout "qwerty")
;;  (setq xah-fly-use-control-key nil)
;;  :init
;;  (xah-fly-keys 1))

;; Doom Themes
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-molokai t))

;; Counsel
(use-package counsel
  :after ivy
  :ensure t
  :config (counsel-mode))

;; Ivy
(use-package ivy
  :defer 0.1
  :diminish
  :ensure t
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config
  (setq ivy-re-builders-alist
      '((swiper . ivy--regex-plus)
        (t      . ivy--regex-fuzzy)))
  :init
  (ivy-mode))

;; Ivy Rich
(use-package ivy-rich
  :after ivy
  :ensure t
  :custom
  (ivy-virtual-abbreviate 'full
                          ivy-rich-switch-buffer-align-virtual-buffer t
                          ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer)
  :init
  (ivy-rich-mode 1))

;; flx to fix fuzzy matching
(use-package flx
  :ensure t)

;; Swiper
(use-package swiper
  :after ivy
  :ensure t)

;; Avy
(use-package avy
  :ensure t)

;; Ranger
(use-package ranger
  :ensure t
  :config
  (setq ranger-cleanup-eagerly t))

;; All The Icons
(use-package all-the-icons
  :ensure t)

;; Mac Terminal Fixes
(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :ensure t
    :config
    (exec-path-from-shell-initialize)))

;; NeoTree
(use-package neotree
  :ensure t
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

;; Org Brain
(use-package org-brain
  :ensure t)

;; Magit
(use-package magit
  :ensure t)

;; Git Timemachine
(use-package git-timemachine
  :ensure t)

;; Which Key
(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode))

 ;; General (custom keybindings)
(use-package general
  :after which-key
  :ensure t
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "TAB" '(switch-to-other-buffer :which-key "previous buffer")
   "SPC" '(counsel-M-x :which-key "M-x")
   "/"   '(counsel-rg :which-key "ripgrep")

   ;; Projects
   "p"   '(:ignore t :which-key "Projects")
   "pf"  '(counsel-git :which-key "search project")
   "ps"  '(:ignore :which-key "switch project")
   "pd"  '(:ignore :which-key "kill project")
   "pa"  '(:ignore :which-key "add buffer to project")
   "pn"  '(:ignore :which-key "rename project")
   "pr"  '(:ignore :which-key "remove buffer from project")
   "pn"  '(:ignore :which-key "next project")
   "pp"  '(:ignore :which-key "previous project")

   ;; Buffers
   "b"   '(:ignore t :which-key "Buffers")
   "bb"  '(counsel-switch-buffer :which-key "switch buffer")
   "bo"  '(counsel-switch-buffer-other-window :which-key "bb other window")
   "bd"  '(kill-this-buffer :which-key "kill buffer")

   ;; Windows
   "w"   '(:ignore t :which-key "Windows")
   "wl"  '(windmove-right :which-key "move right")
   "wh"  '(windmove-left :which-key "move left")
   "wk"  '(windmove-up :which-key "move up")
   "wj"  '(windmove-down :which-key "move bottom")
   "w/"  '(split-window-right :which-key "split right")
   "w-"  '(split-window-below :which-key "split bottom")
   "wd"  '(delete-window :which-key "delete window")

   ;; Search
   "s"   '(:ignore t :which-key "Search")
   "sw"  '(avy-goto-word-0 :which-key "go to word")
   "sb"  '(swiper :which-key "search buffer")
   "sr"  '(counsel-recentf :which-key "search recent")
   "sf"  '(counsel-git-grep :which-key "search repo")
   "sa"  '(counsel-ag :which-key "ag")
   "sc"  '(counsel-yank-pop :which-key "Copy/Paste")

   ;; Themes
   "t"   '(:ignore t :which-key "Themes")
   "ts"  '(counsel-load-theme :which-key "load theme")

   ;; Files
   "f"   '(:ignore t :which-key "Files")
   "ff"  '(counsel-find-file :which-key "find file")

   ;; Git
   "g"   '(:ignore t :which-key "Git")
   "gs"  '(:ignore t :which-key "status")
   "gt"  '(:ignore t :which-key "time machine")

   ;; Org Mode
   "o"   '(:ignore t :which-key "Org")
   "oa"  '(org-agenda :which-key "agenda")
   "ob"  '(org-brain-visualize :which-key "brain")
   
   ;; Applications
   "a"   '(:ignore t :which-key "Applications")
   "at"  '(ansi-term :which-key "open terminal")
   "ar"  '(ranger :which-key "ranger")

   ;; Emacs
   "e"   '(:ignore t :which-key "Emacs")
   "eq"  '(:ignore :which-key "quit?")
   "eQ"  '(kill-emacs :which-key "QUIT!")
   "er"  '(:ignore t :which-key "restart?")
   "eR"  '(:ignore t :which-key "RESTART!")
   "ep"  '(counsel-list-processes :which-key "list processes")
   
   ;; Help
   "h"   '(:ignore t :which-key "Help")
   "hf"  '(counsel-describe-function :which-key "describe function")
   "hv"  '(counsel-describe-variable :which-key "describe variable")
   "hk"  '(counsel-descbinds :which-key "describe keybindings")
   ))

;; Flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; LSP
(use-package lsp-mode
  :ensure t
  :init
  (add-hook 'prog-major-mode #'lsp-prog-major-mode-enable))

(use-package lsp-ui
  :ensure t
  :init
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; Company mode
(use-package company
  :ensure t
  :init
  (setq company-minimum-prefix-length 3)
  (setq company-auto-complete nil)
  (setq company-idle-delay 0)
  (setq company-require-match 'never)
  (setq company-frontends
  '(company-pseudo-tooltip-unless-just-one-frontend
    company-preview-frontend
    company-echo-metadata-frontend))
  (setq tab-always-indent 'complete)
  (defvar completion-at-point-functions-saved nil)
  :config
  (global-company-mode 1)
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  (define-key company-mode-map [remap indent-for-tab-command] 'company-indent-for-tab-command)
  (defun company-indent-for-tab-command (&optional arg)
    (interactive "P")
  (let ((completion-at-point-functions-saved completion-at-point-functions)
    	(completion-at-point-functions '(company-complete-common-wrapper)))
    (indent-for-tab-command arg)))

  (defun company-complete-common-wrapper ()
    (let ((completion-at-point-functions completion-at-point-functions-saved))
      (company-complete-common))))

(use-package company-lsp
  :ensure t
  :init
  (push 'company-lsp company-backends))
 
;; Powerline
(use-package spaceline
  :ensure t
  :init
  (setq powerline-default-separator 'slant)
  :config
  (spaceline-emacs-theme)
  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-buffer-size-off)
  (spaceline-toggle-evil-state-on))

;;;;;;;;;;;;;;;;;;;;;;;
;; Language Supports ;;
;;;;;;;;;;;;;;;;;;;;;;;
 
;; JavaScript
(use-package js2-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))
(use-package tern :ensure t)

;; Rust
(use-package rust-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)))

(use-package lsp-rust
  :ensure t
  :init
  (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls"))
  (add-hook 'rust-mode-hook #'lsp-rust-enable)
  (add-hook 'rust-mode-hook #'flycheck-mode))

 ;; Typescript
(use-package typescript-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode)))

;; LSP for JavaScript and TypeScript
(use-package lsp-javascript-typescript
  :ensure t
  :init
  (add-to-list 'js-mode-hook #'lsp-javascript-typescript-enable)
  (add-to-list 'typescript-mode-hook #'lsp-javascript-typescript-enable))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (2048-game magit flx ivy-hydra xah-fly-keys which-key use-package tern spaceline ranger neotree lsp-ui lsp-rust lsp-javascript-typescript js2-mode ivy-rich helm general flycheck exec-path-from-shell evil doom-themes counsel company-lsp avy))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
