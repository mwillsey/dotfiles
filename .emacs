;; max's emacs.d

;; melpa
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; a bunch of quick one line fixes
(tool-bar-mode -1)                    ;; fix ui
(scroll-bar-mode -1)
(setq mouse-wheel-scroll-amount '(1)) ;; fix scroll
(setq mouse-wheel-progressive-speed nil)
(pending-delete-mode t)               ;; type over region 
(winner-mode 1)                       ;; undo window changes
(setq exec-path (cons "/usr/local/bin/" exec-path))
(global-auto-revert-mode 1)
(setq-default indent-tabs-mode nil)
(column-number-mode t)
(toggle-uniquify-buffer-names t)
(setq mac-command-modifier 'meta)
(setq inhibit-splash-screen t)
(setq initial-scratch-message "")
(setq initial-major-mode 'text-mode)
(load-theme 'misterioso t)
(setq ispell-program-name "aspell")

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

;; org setup
(add-hook 'org-mode-hook
          (lambda ()
            (visual-line-mode 1)
            (org-indent-mode 1)
            (flyspell-mode 1)))

(setq org-directory "~/Dropbox/org/")

(setq org-default-notes-file (concat org-directory "notes.org"))
(define-key global-map (kbd "C-c c") 'org-capture)

(setq org-capture-templates 
      '(("w" "Writing" entry 
         (file "~/Dropbox/org/writing.org") 
         "* %t \n%?"
         :emptylines 1) 
        ("r" "To Read" entry 
         (file+headline "~/Dropbox/org/reading.org" "To Read") 
         ""
         :emptylines 1) 
        ("n" "Note" entry 
         (file+headline "~/Dropbox/org/notes.org" "Inbox") 
         ""
         :emptylines 1) 
        ("t" "Todo" entry 
         (file+headline "~/Dropbox/org/todo.org" "Inbox") 
         "* TODO %?"
         :emptylines 1)))
