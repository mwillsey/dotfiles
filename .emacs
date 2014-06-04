;; max's work init.el

;; melpa
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; commmand as meta
(setq mac-command-modifier 'meta)

;; fix paths
(setq exec-path (cons "/usr/local/bin/" exec-path))

;; get rid of scroll and tool bar
(tool-bar-mode -1) 
(scroll-bar-mode -1)

;; scrolling
(setq mouse-wheel-scroll-amount '(1))
(setq mouse-wheel-progressive-speed nil)

;; enable typing over selected region
(pending-delete-mode t)

;; winner mode for undoing window changes
(winner-mode 1)

;; ido mode
(ido-mode 1) 

;; wrapping
(global-visual-line-mode 1)

;; inhibit startup screen
(setq inhibit-splash-screen t)

;; change scratch buffer
(setq initial-scratch-message "")
(setq initial-major-mode 'text-mode)

;; no tabs
(setq-default indent-tabs-mode nil)

;; show column number
(column-number-mode t)

;; unique buffer names
(toggle-uniquify-buffer-names t)


;; theming
(load-theme 'misterioso t)

;; show matching parens
(show-paren-mode 1)
(setq show-paren-delay 0)
(set-face-background 'show-paren-match nil)
(set-face-foreground 'show-paren-match "cyan")

;; evil
(require 'evil)
(evil-mode 1)

;; evil key setup
;; make evil respect visual lines
(define-key evil-motion-state-map "j" #'evil-next-visual-line)
(define-key evil-motion-state-map "k" #'evil-previous-visual-line)
(define-key evil-motion-state-map "$" #'evil-end-of-visual-line)
(define-key evil-motion-state-map "^" #'evil-first-non-blank-of-visual-line)
(define-key evil-motion-state-map "0" #'evil-beginning-of-visual-line)

;; use aspell
(setq ispell-program-name "aspell")

;; org setup
(add-hook 'org-mode-hook
          (lambda ()
            (visual-line-mode 1)
            (org-indent-mode 1)
            (flyspell-mode 1)))
