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
(use-package diminish)

;; setup path
(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

(use-package evil
  ;; evil provides vim-like interaction with emacs
  :init (setq evil-want-C-u-scroll t)
  :ensure t
  :diminish undo-tree-mode
  :config 
    (evil-mode 1)
    (define-key evil-ex-map "e " 'ido-find-file)
    (define-key evil-ex-map "b " 'ido-switch-buffer)
    (define-key evil-visual-state-map (kbd "RET") 'align-regexp)
    (use-package evil-commentary
      :ensure t
      :diminish evil-commentary-mode
      :config (evil-commentary-mode)))


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
  :diminish guide-key-mode
  :config
  (setq guide-key/guide-key-sequence t)
  (guide-key-mode 1))

(use-package tuareg-mode
  ;; tuareg is an ocaml editing mode
  :ensure tuareg)

;; a bunch of quick one line fixes
(setq inhibit-splash-screen t        
      initial-scratch-message "")
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-auto-revert-mode 1)
(setq-default indent-tabs-mode nil)
(column-number-mode t)
(load-theme 'whiteboard t)
(setq default-directory "~")
(setq global-visual-line-mode 1)

;; mac setup
(setq mac-option-modifier 'super
      mac-command-modifier 'meta)

;; scrolling
(setq scroll-step 1
      mouse-wheel-scroll-amount '(2)
      mouse-wheel-progressive-speed nil)

;; show matching parens
(show-paren-mode 1)

;; centralized backups
(setq backup-directory-alist `(("." . "~/.emacs.saves")))

(use-package ido
  ;; ido mode
  :config 
  (ido-mode 1))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (exec-path-from-shell writeroom-mode use-package tuareg guide-key evil-commentary))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
