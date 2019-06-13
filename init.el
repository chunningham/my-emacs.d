;; Package configs
(require 'package)
(setq package-enable-at-startup nil)
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

;; Doom Themes
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-molokai t))

;; Helm
(use-package helm
  :ensure t
  :init
  (setq helm-mode-fuzzy-match t)
  (setq helm-completion-in-region-fuzzy-match t)
  (setq helm-candidate-number-list 50))

;; All The Icons
(use-package all-the-icons :ensure t)

;; Which Key
(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode))

;; General (custom keybinding
(use-package general
  :ensure t
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   ;; "/" '(config-rg :which-key "ripgrep")
   "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
   "SPC" '(helm-M-x :which-key "M-x")
   "pf"  '(helm-find-file :which-key "find files")

   ;; Buffers
   "bb"  '(helm-buffers-list :which-key "buffers list")
   "bd"  '(kill-this-buffer :which-key "kill buffer")

   ;; Windows
   "wl"  '(windmove-right :which-key "move right")
   "wh"  '(windmove-left :which-key "move left")
   "wk"  '(windmove-up :which-key "move up")
   "wj"  '(windmove-down :which-key "move bottom")
   "w/"  '(split-window-right :which-key "split right")
   "w-"  '(split-window-below :which-key "split bottom")
   "wx"  '(delete-window :which-key "delete window")

   ;; Others
   "at"  '(ansi-term :which-key "open terminal")
   ))




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a8c210aa94c4eae642a34aaf1c5c0552855dfca2153fa6dd23f3031ce19453d4" default)))
 '(package-selected-packages
   (quote
    (general which-key helm use-package evil doom-themes))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
