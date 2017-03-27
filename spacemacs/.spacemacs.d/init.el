;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.

(defconst mw/directory
  (expand-file-name
   (if (eq system-type 'darwin)
     "~/Dropbox" "~")))

(defun mw/dir (&rest paths)
  (mapconcat 'identity (cons mw/directory paths) "/"))

(defun mw/take-face-attribute (dst src &rest attrs)
  (dolist (attr attrs)
    (set-face-attribute dst nil attr
                        (face-attribute src attr))))

(define-minor-mode prose-mode
  "A sane environment for writing long (non-wrapped) lines of text."
  nil nil nil
  (if prose-mode
    (progn
      (auto-fill-mode -1)
      (spacemacs/toggle-visual-line-navigation-on)
      (spacemacs/toggle-spelling-checking-on)
      (olivetti-mode 1)
      (variable-pitch-mode 1))
    (progn
      (auto-fill-mode 1)
      (spacemacs/toggle-visual-line-navigation-off)
      (spacemacs/toggle-spelling-checking-off)
      (olivetti-mode -1)
      (variable-pitch-mode -1))))

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(
     ;; system
     (shell :variables shell-default-shell 'eshell)
     osx

     ;; emacs
     emacs-lisp
     helm
     org

     ;; tools
     git
     github
     vagrant
     graphviz

     ;; editing
     spell-checking

     ;; tex
     bibtex
     (latex :variables
            latex-enable-auto-fill nil
            latex-enable-folding t)

     ;; c
     (c-c++ :variables c-c++-enable-clang-support t)
     semantic
     gtags

     ;; web
     html
     markdown

     ;; other langs
     racket
     python
     )
   dotspacemacs-additional-packages '(olivetti
                                      boogie-friends
                                      arduino-mode
                                      evil-terminal-cursor-changer)
   dotspacemacs-frozen-packages '()
   dotspacemacs-install-packages 'used-only
   dotspacemacs-excluded-packages '(vi-tilde-fringe
                                    evil-tutor
                                    powerline)))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update nil
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner nil
   dotspacemacs-startup-lists '()
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes '(spacemacs-light
                         spacemacs-dark)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Inconsolata"
                               :size 13)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab t
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text nil
   dotspacemacs-ex-substitute-global t
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-helm-use-fuzzy 'source
   dotspacemacs-enable-paste-transient-state t
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native t
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 100
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers nil
   dotspacemacs-folding-method 'evil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis t
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup 'trailing
   ))

(defun dotspacemacs/user-init ()

  ;; stop spacemacs from complaining about me setting my path
  (setq exec-path-from-shell-check-startup-files nil)

  ;; Appearance and theme
  (setq-default spacemacs-theme-org-agenda-height nil
                spacemacs-theme-org-height nil
                spacemacs-theme-comment-bg nil))

(defun dotspacemacs/user-config ()

  (add-to-list 'load-path (expand-file-name "lisp" dotspacemacs-directory))

  (spacemacs/set-leader-keys
    "RET" 'helm-mini
    "aos" 'helm-multi-swoop-org
    "TV"  'variable-pitch-mode)

  (setq helm-echo-input-in-header-line nil)

  ; spacemacs commented this out for now
  (unless (display-graphic-p)
    (spacemacs-evil/init-evil-terminal-cursor-changer)
    (evil-terminal-cursor-changer-activate))

  (setq powerline-default-separator 'bar)
  (spaceline-compile)

  (with-eval-after-load 'osx-dictionary
    (add-hook 'osx-dictionary-mode-hook 'mw/enable-writing-long-lines))

  (use-package llvm-mode
    :load-path "~/src/llvm/utils/emacs")

  ;; boogie set up
  (setq flycheck-dafny-executable "/Users/mwillsey/src/dafny/dafny")
  ;; (setq flycheck-boogie-executable "PATH-TO-BOOGIE")
  ;; (setq flycheck-z3-smt2-executable "z3")
  (setq flycheck-inferior-dafny-executable "/Users/mwillsey/src/dafny/dafny-server")

  ;; browse URLs (including mail) externally
  (setq browse-url-mailto-function  'browse-url-generic
        browse-url-browser-function 'browse-url-generic)
  (case system-type
    (darwin
     (setq browse-url-generic-program "open"))
    (otherwise
     (setq browse-url-generic-program "client"
           browse-url-generic-args '("open"))))

  (setq mouse-wheel-scroll-amount '(2 ((shift) . 5))
        mouse-wheel-progressive-speed nil)

  (with-eval-after-load 'flyspell
    (define-key flyspell-mouse-map (kbd "<mouse-3>") 'flyspell-correct-word))

  (with-eval-after-load 'ggtags
    (define-key ggtags-mode-map [mouse-3] 'ggtags-find-tag-mouse))

  (dolist (m (list evil-normal-state-map evil-motion-state-map))
    (bind-keys :map m
               ("H" . evil-first-non-blank)
               ("L" . evil-end-of-line)))
  (with-eval-after-load 'latex

    (add-hook 'TeX-mode-hook 'prose-mode)

    (setq TeX-engine 'xetex
          TeX-view-program-selection '((output-dvi "open")
                                       (output-pdf "displayline")
                                       (output-html "open")))
    (case system-type
      (darwin
       (setq TeX-view-program-list '(("open" "open %o")
                                     ("displayline" "displayline -b -g %n %o %b"))))
      (otherwise
       (setq TeX-view-program-list '(("open" "client open %o")
                                     ("displayline" "client 'displayline -b -g %n' %o %b")))))

    (add-to-list 'LaTeX-verbatim-macros-with-delims "lstinline")

    (setq TeX-auto-local  ".auctex.auto"
          TeX-style-local ".auctex.style"))

  (require 'init-org)

  (setq gc-cons-threshold (* 800 1024)))



(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(spacemacs-theme-comment-bg nil)
 '(spacemacs-theme-org-agenda-height nil)
 '(spacemacs-theme-org-height nil)
 '(spacemacs-theme-org-highlight nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(variable-pitch ((t (:family "Helvetica Neue Light")))))
)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ivy helm-bibtex parsebib avy packed company yasnippet flycheck smartparens evil helm helm-core markdown-mode projectile magit magit-popup git-commit hydra vi-tilde-fringe evil-tutor define-word yapfify xterm-color ws-butler window-numbering which-key web-mode volatile-highlights vagrant-tramp vagrant uuidgen use-package toc-org tagedit stickyfunc-enhance srefactor spacemacs-theme spaceline smeargle slim-mode shell-pop scss-mode sass-mode reveal-in-osx-finder restart-emacs rainbow-delimiters racket-mode quelpa pyvenv pytest pyenv-mode py-isort pug-mode popwin pip-requirements persp-mode pcre2el pbcopy paradox osx-trash osx-dictionary orgit org-ref org-projectile org-present org-pomodoro org-plus-contrib org-download org-bullets open-junk-file olivetti neotree multi-term move-text mmm-mode markdown-toc magit-gitflow magit-gh-pulls macrostep lorem-ipsum live-py-mode linum-relative link-hint less-css-mode launchctl info+ indent-guide ido-vertical-mode hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make helm-gtags helm-gitignore helm-flx helm-descbinds helm-css-scss helm-ag graphviz-dot-mode google-translate golden-ratio gnuplot github-search github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gist gh-md ggtags flyspell-correct-helm flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-terminal-cursor-changer evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help emmet-mode elisp-slime-nav dumb-jump disaster cython-mode column-enforce-mode cmake-mode clean-aindent-mode clang-format boogie-friends auto-highlight-symbol auto-dictionary auto-compile auctex-latexmk arduino-mode anaconda-mode aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
