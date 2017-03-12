;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.

(defconst mw/directory "~/Dropbox")
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
                                      arduino-mode)
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

  (spacemacs/set-leader-keys
    "RET" 'helm-mini
    "aos" 'helm-multi-swoop-org
    "TV"  'variable-pitch-mode)

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
  (setq browse-url-mailto-function 'browse-url-generic
        browse-url-generic-program "open")

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
                                       (output-html "open"))
          TeX-view-program-list '(("open" "client open %o")
                                  ("displayline" "client 'displayline -b -g %n' %o %b")))

    (add-to-list 'LaTeX-verbatim-macros-with-delims "lstinline")

    (setq TeX-auto-local  ".auctex.auto"
          TeX-style-local ".auctex.style"))

  (defun mw/load-org ()
    (require 'org)
    (require 'org-agenda))
  (run-with-idle-timer 5 nil 'mw/load-org)

  (with-eval-after-load 'org
    (message "Loading org...")

    (add-hook 'org-mode-hook 'mw/org-mode-hook)
    (defun mw/org-mode-hook ()
      (auto-fill-mode 1))

    ;; put my stuff in the right location
    ;; disable archiving for now
    (setq org-directory mw/directory
          org-default-notes-file (mw/dir "inbox.org")
          org-archive-location nil)

    ;; I like chronological from the top down
    (setq org-reverse-note-order '(("inbox.org" . nil)
                                   ("."         . t)))

    ;; refile can use full paths because we use completion
    (setq org-outline-path-complete-in-steps nil
          org-refile-use-outline-path 'file
          org-refile-targets '((org-agenda-files :maxlevel . 9)))

    (require 'org-mac-link)
    (spacemacs/set-leader-keys-for-major-mode 'org-mode "ig" 'org-mac-grab-link)

    ;; set up a more modest appearance
    (setq org-adapt-indentation nil
          org-level-color-stars-only t
          org-ellipsis "…"
          org-startup-indented t)

    ;; dim drawers and stuff, they are too bold as-is
    (mw/take-face-attribute 'org-special-keyword 'shadow :foreground)

    ;; put everything into a drawer
    (setq org-log-done nil
          org-log-into-drawer t)

    (setq org-todo-keywords '((sequence
                               "TODO(t@)" "NEXT(n!)" "HOLD(h@/!)"
                               "|"
                               "DONE(d!)" "DROP(c@/!)")
                              (type "MEETING(m)"))
          org-todo-keyword-faces '(("TODO" . "red1")
                                   ("NEXT" . "magenta1")
                                   ("DONE" . "green4")
                                   ("HOLD" . "gold3")
                                   ("DROP" . "blue3")))

    (setq org-capture-templates
          '(("t" "todo" entry (file org-default-notes-file)
             "* TODO %?\n")
            ("j" "journal" entry (file "journal.org")
             "* %?\n")
            ("m" "mail" entry (file org-default-notes-file)
             "* TODO %a\nSCHEDULED: %t\n%?")
            (" " "blank" entry (file org-default-notes-file)
             "* %?\n")))

    (add-hook 'org-capture-mode-hook 'evil-insert-state)

    ;; agenda files are either at the top level or in those folders
    (setq org-agenda-files
          (cons org-directory
                (mapcar 'mw/dir (list "work/" "personal/" "settings/"))))

    ;; (add-hook 'org-agenda-mode-hook 'mw/org-agenda-mode-hook)
    ;; (defun mw/org-agenda-mode-hook ()
    ;;   (olivetti-mode     1)
    ;;   (visual-line-mode -1)
    ;;   (olivetti-set-width 85))

    ;; clean 3 day view, only show the time grid on a single day
    (setq org-agenda-span 3
          org-agenda-window-setup 'current-window
          org-agenda-restore-windows-after-quit nil
          org-agenda-skip-timestamp-if-done nil
          org-agenda-skip-scheduled-if-done t
          org-agenda-skip-scheduled-if-deadline-is-shown t
          org-agenda-skip-deadline-if-done nil
          org-agenda-skip-deadline-prewarning-if-scheduled t
          org-agenda-time-grid '((daily require-timed)
                                 "----------------"
                                 (800 1000 1200 1400 1600 1800 2000)))

    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "r" 'org-refile)
    (spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
      "r" 'org-agenda-refile)

    ;; agenda should make sure to include things that need to be refiled
    (setq org-agenda-custom-commands
          '(("a" "Agenda"
             ((tags "refile"
                    ((org-agenda-overriding-header "Refile")
                     (org-tags-match-list-sublevels t)))
              (agenda "" nil))
             nil
             ("~/Dropbox/org/agenda.html"))))

    ;; disable org attach for now
    (setq org-attach-directory nil)

    ;; clean up the default export settings
    (setq org-export-with-toc nil
          org-export-with-section-numbers nil
          org-export-with-tags nil
          org-export-with-todo-keywords nil
          org-export-with-drawers nil
          org-export-with-smart-quotes t)

    (require 'ox-latex)
    (add-to-list 'org-latex-packages-alist '("newfloat" "minted"))
    (setq org-latex-pdf-process '("latexmk -pdf -shell-escape -output-directory=%o %f")
          org-latex-listings 'minted)

    (require 'ox-extra)
    (ox-extras-activate '(ignore-headlines))

    (setq org-html-head "
<style type=\"text/css\">
#content {
  max-width: 850px;
  margin: 0 auto;
}

#postamble { opacity: .5; }
</style>")

    (require 'org-ref)

    (setq org-ref-bibliography-notes     (mw/dir "work/papers.org")
          org-ref-default-bibliography   (list (mw/dir "work/papers/papers.bib"))
          org-ref-pdf-directory          (mw/dir "work/papers/pdfs/")
          reftex-default-bibliography    org-ref-default-bibliography
          bibtex-completion-bibliography org-ref-default-bibliography
          bibtex-completion-library-path org-ref-pdf-directory
          bibtex-completion-notes-path   org-ref-bibliography-notes)

    (setq bibtex-completion-pdf-open-function
          (lambda (fpath)
            (start-process "open" "*open*" "open" fpath)))

    (setq reftex-cite-punctuation '(", " " and " " {\\it et al.}")H)
    (setq org-ref-note-title-format
          (mapconcat 'identity
                     '("* %t"
                       ":PROPERTIES:"
                       ":Custom_ID: %k"
                       ":Title:     %t"
                       ":Author:    %a"
                       ":Journal:   %j"
                       ":Year:      %y"
                       ":Volume:    %v"
                       ":Pages:     %p"
                       ":DOI:       %D"
                       ":URL:       %U"
                       ":END:")))


    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (shell . t)
       (sql . t)
       (dot . t)
       (python . t)))

    (setq org-edit-src-content-indentation 0)

    (setq org-babel-default-header-args:sql
          '((:engine . "postgresql")
            (:dbhost . "localhost")
            (:dbuser . "mwillsey"))))
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
    (powerline faceup spinner org org-plus-contrib hydra parent-mode request haml-mode gitignore-mode gh marshal logito pcache ht flyspell-correct flx magit-popup with-editor iedit anzu company yasnippet pkg-info epl bind-map biblio-core packed pythonic f dash s avy popup package-build auctex-latexmk smartparens projectile web-mode use-package srefactor shell-pop restart-emacs pyvenv persp-mode org-ref ivy mmm-mode markdown-toc live-py-mode info+ indent-guide hide-comnt help-fns+ helm-bibtex git-link expand-region evil-nerd-commenter ace-window flycheck highlight evil helm helm-core alert magit async yapfify xterm-color ws-butler window-numbering which-key volatile-highlights vagrant-tramp vagrant uuidgen undo-tree toc-org tagedit stickyfunc-enhance spacemacs-theme spaceline smeargle slim-mode scss-mode sass-mode reveal-in-osx-finder rainbow-delimiters racket-mode quelpa pytest pyenv-mode py-isort pug-mode popwin pip-requirements pcre2el pbcopy parsebib paradox osx-trash osx-dictionary orgit org-projectile org-present org-pomodoro org-download org-bullets open-junk-file olivetti neotree multi-term move-text markdown-mode magit-gitflow magit-gh-pulls macrostep lorem-ipsum log4e linum-relative link-hint less-css-mode launchctl key-chord ido-vertical-mode hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make helm-gtags helm-gitignore helm-flx helm-descbinds helm-css-scss helm-ag graphviz-dot-mode goto-chg google-translate golden-ratio gnuplot gntp github-search github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-commit gist gh-md ggtags flyspell-correct-helm flx-ido fill-column-indicator fancy-battery eyebrowse exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-surround evil-search-highlight-persist evil-numbers evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help emmet-mode elisp-slime-nav dumb-jump disaster diminish cython-mode column-enforce-mode cmake-mode clean-aindent-mode clang-format boogie-friends bind-key biblio auto-highlight-symbol auto-dictionary auto-compile auctex arduino-mode anaconda-mode aggressive-indent adaptive-wrap ace-link ace-jump-helm-line))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
