;;; max's emacs.d 

;; init package management
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))


(use-package evil
  ;; evil provides vim-like interaction with emacs
  :ensure t
  :config 
    (evil-mode 1)
    (use-package evil-commentary
      :ensure t
      :config
      (evil-commentary-mode))
    (define-key evil-visual-state-map (kbd "RET") 'align-regexp))

(use-package org
  ;; org is an organizational suite
  :ensure t)

(use-package flyspell
  :config
  (add-hook 'org-mode-hook 'turn-on-flyspell)
  (setq ispell-program-name "aspell"))

(use-package writeroom-mode
  ;; writeroom-mode starts a distraction-free writing enviroment
  :ensure t
  :config 
  (setq writeroom-global-effects '(writeroom-toggle-alpha
                                   writeroom-toggle-menu-bar-lines
                                   writeroom-toggle-tool-bar-lines
                                   writeroom-toggle-vertical-scroll-bars
                                   writeroom-toggle-internal-border-width)
        writeroom-major-modes '(text-mode org-mode)
        writeroom-maximize-window nil
        writeroom-fringes-outside-margins nil))

(use-package guide-key
  ;; guide-key pops up keybindings once you've started a command
  :ensure t
  :config
  (setq guide-key/guide-key-sequence t)
  (guide-key-mode 1))

;; a bunch of quick one line fixes
(setq inhibit-splash-screen t)        
(setq initial-scratch-message "")
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-auto-revert-mode 1)
(setq-default indent-tabs-mode nil)
(column-number-mode t)
(load-theme 'tango t)
(setq default-directory "~")
(global-visual-line-mode 1)

;; show matching parens
(show-paren-mode 1)
; (setq show-paren-delay 0)
; (set-face-background 'show-paren-match nil)
; (set-face-foreground 'show-paren-match "cyan")

;; centralized backups
(setq backup-directory-alist `(("." . "~/.emacs.saves")))

;; ido mode
(ido-mode 1) 
(define-key evil-ex-map "e " 'ido-find-file)
(define-key evil-ex-map "b " 'ido-switch-buffer)
