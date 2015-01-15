;;; max's emacs.d 

;; package management
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")))
(package-initialize)

;; a bunch of quick one line fixes
(setq inhibit-splash-screen t)        
(setq initial-scratch-message "")
(tool-bar-mode -1)                    ;; fix ui
(scroll-bar-mode -1)                  ;; fix scroll
(setq mouse-wheel-scroll-amount '(1)) 
(setq mouse-wheel-progressive-speed nil)
(pending-delete-mode t)               ;; type over region 
(winner-mode 1)                       ;; undo window changes
(global-auto-revert-mode 1)
(setq-default indent-tabs-mode nil)
(column-number-mode t)
(toggle-uniquify-buffer-names t)
(load-theme 'misterioso t)
(setq ispell-program-name "aspell")
(setq default-directory "~")
(global-visual-line-mode 1)

;; setup PATH
(let ((path-from-shell
       (replace-regexp-in-string
        "[[:space:]\n]*$" ""
        (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
  (setenv "PATH" path-from-shell)
  (setq exec-path (split-string path-from-shell path-separator)))

;; show matching parens
(show-paren-mode 1)
(setq show-paren-delay 0)
(set-face-background 'show-paren-match nil)
(set-face-foreground 'show-paren-match "cyan")

;; centralized backups
(setq backup-directory-alist `(("." . "~/.emacs.saves")))

;; evil
(require 'evil)
(evil-mode 1)

;; evil key setup - make evil respect visual lines
(define-key evil-motion-state-map "j" #'evil-next-visual-line)
(define-key evil-motion-state-map "k" #'evil-previous-visual-line)
(define-key evil-motion-state-map "$" #'evil-end-of-visual-line)
(define-key evil-motion-state-map "^" #'evil-first-non-blank-of-visual-line)
(define-key evil-motion-state-map "0" #'evil-beginning-of-visual-line)

(evil-ex-define-cmd "sh" 'shell)

;; ido mode
(ido-mode 1) 
(define-key evil-ex-map "e " 'ido-find-file)
(define-key evil-ex-map "b " 'ido-switch-buffer)

;;; LaTeX setup

(add-hook 'LaTeX-mode-hook 
          (lambda ()
            (flyspell-mode 1)))

(setq TeX-PDF-mode t)

;; no more "error occured after last TeX file closed
(setq LaTeX-command-style (quote (("" "%(PDF)%(latex) -file-line-error %S%(PDFout)"))))

;; use Skim as default pdf viewer
;; Skim's displayline is used for forward search (from .tex to .pdf)
;; option -b highlights the current line; option -g opens Skim in the background  
(setq TeX-view-program-selection '((output-pdf "Skim")))
(setq TeX-view-program-list
      '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))
