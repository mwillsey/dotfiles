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
   dotspacemacs-distribution 'spacemacs-base
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(;; system
     (shell :variables
            shell-default-shell 'shell)
     osx

     ;; emacs
     emacs-lisp
     helm
     org
     mw-org ;; private

     ;; manually pick some of the spacemacs layers because I'm using
     ;; spacemacs-base, but leave out some
     spacemacs-editing
     spacemacs-evil
     spacemacs-completion
     spacemacs-org

     ;; tools
     git
     github
     vagrant
     graphviz
     neotree

     ;; editing
     auto-completion
     syntax-checking
     spell-checking

     ;; tex
     bibtex
     (latex :variables
            latex-enable-auto-fill nil
            latex-enable-folding t)

     ;; c
     (c-c++ :variables
            c-c++-enable-clang-support t)

     ;; web
     html
     markdown
     javascript
     typescript

     ;; other langs
     ocaml
     racket
     python
     yaml
     sql
     major-modes ;; scad, etc.
     lean
     )

   dotspacemacs-additional-packages '(olivetti
                                      default-text-scale)
   dotspacemacs-frozen-packages '()
   dotspacemacs-install-packages 'used-but-keep-unused
   dotspacemacs-excluded-packages '(vi-tilde-fringe
                                    evil-tutor
                                    yasnippet
                                    wolfram-mode ;; from major-modes
                                    )))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-verify-spacelpa-archives t
   dotspacemacs-check-for-update t
   dotspacemacs-elpa-subdirectory 'emacs-version
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner nil
   dotspacemacs-startup-lists '()
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes '(leuven
                         spacemacs-light
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
   dotspacemacs-loading-progress-bar nil
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native t
   dotspacemacs-maximized-at-startup t
   dotspacemacs-active-transparency 100
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers nil
   dotspacemacs-folding-method 'evil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-frame-title-format "%I@%S"
   dotspacemacs-icon-title-format nil
   dotspacemacs-whitespace-cleanup 'trailing
   dotspacemacs-zone-out-when-idle nil
   dotspacemacs-pretty-docs nil
   ))

(defun dotspacemacs/user-init ()

  (add-to-list 'exec-path "/usr/local/bin")

  ;; set the location of various files and folders needed for setup
  (setq
   custom-file (expand-file-name "custom-settings.el" spacemacs-cache-directory)
   dotspacemacs-configuration-layer-path
               (list (expand-file-name "layers/" dotspacemacs-directory))
   mw/lisp-dir (expand-file-name "lisp/" dotspacemacs-directory))

  ;; set loadpath to mw/lisp-dir and its immediate subdirectories
  (add-to-load-path mw/lisp-dir)
  (let ((default-directory mw/lisp-dir))
    (normal-top-level-add-subdirs-to-load-path))

  ;; stop spacemacs from complaining about me setting my path
  (setq exec-path-from-shell-check-startup-files nil)

  ;; ignore certain file extensions when looking for files
  ;; needs to be run before helm loads so it can see this when making
  ;; helm-boring-file-regex-list
  (dolist (pat '(".dropbox" ".dropbox.cache" ".synctex.gz" ".DS_Store"))
    (add-to-list 'completion-ignored-extensions pat))

  ;; Appearance and theme
  (setq-default spacemacs-theme-org-agenda-height nil
                spacemacs-theme-org-height nil
                spacemacs-theme-comment-bg nil))

(defun dotspacemacs/user-config ()

  (global-set-key (kbd "H-h") 'ns-do-hide-emacs)
  (evil-global-set-key 'insert (kbd "C-SPC") 'helm-company)

  (spacemacs/set-leader-keys
    "RET" 'helm-mini
    "p RET" 'helm-projectile
    "aos" 'helm-multi-swoop-org
    "TV"  'variable-pitch-mode)

  (setq eshell-cmpl-ignore-case t
        pcomplete-ignore-case t)

  ;; don't show garbage in my find-file
  (setq helm-ff-skip-boring-files t)

  ;; olivetti is better
  (spacemacs/set-leader-keys
    "wc" 'olivetti-mode
    "wC" 'olivetti-set-width)

  (setq-default olivetti-body-width 100)

  ;; bind shift-cmd +/- to *globally* modify the font size
  ;; unlike the un-shifted version that is buffer local
  (global-set-key (kbd "H-+") 'default-text-scale-increase)
  (global-set-key (kbd "H-_") 'default-text-scale-decrease)

  ;; spacemacs commented this out for now
  (unless (display-graphic-p)
    (spacemacs-evil/init-evil-terminal-cursor-changer)
    (evil-terminal-cursor-changer-activate))

  ;; sometimes this gets changed without my knowledge, just make sure
  (setq ispell-program-name (executable-find "aspell"))

  ;; please don't ask me, just compile
  (setq compilation-ask-about-save nil)

  (with-eval-after-load 'lua
    (setq lua-indent-level 4))

  ;; make sure to django/liquid/jekyll templating
  (setq web-mode-engines-alist
        '(("django" . "\\.html\\'")))


  (spacemacs/set-leader-keys "bt" 'mw/open-terminal)
  (defun mw/open-terminal ()
    "Open the current directory in a new terminal tab."
    (interactive)
    (ns-do-applescript
     (format "
        tell application \"Terminal\"
          activate
          tell application \"System Events\"
            keystroke \"t\" using {command down}
          end tell
          do script \"cd %s\" in selected tab of the front window
        end tell"
      (expand-file-name default-directory))))

  ;; dont make new frames
  (setq ns-pop-up-frames nil)

  ;; TeX is the only one I really use
  (setq default-input-method "TeX")

  ;; try to split vertically more often
  (setq split-window-preferred-function 'mw/split-window-sensibly)
  (defun mw/split-window-sensibly (&optional window)
    "My version of `split-window-sensibly' that tries horizontal splitting
    before vertical."
    (let ((window (or window (selected-window)))
          (horiz t))
      (or
       (and (window-splittable-p window horiz)
            ;; Split window horizontally.
            (with-selected-window window
              (split-window-right)))
       (and (window-splittable-p window)
            ;; Split window vertically.
            (with-selected-window window
              (split-window-below)))
       (and (eq window (frame-root-window (window-frame window)))
            (not (window-minibuffer-p window))
            ;; If WINDOW is the only window on its frame and is not the
            ;; minibuffer window, try to split it vertically disregarding
            ;; the value of `split-height-threshold'.
            (let ((split-height-threshold 0))
              (when (window-splittable-p window)
                (with-selected-window window
                  (split-window-below))))))))

  (with-eval-after-load 'neotree
    (with-eval-after-load 'helm
      (setq neo-hidden-regexp-list helm-boring-file-regexp-list
            neo-show-hidden-files nil)))

  ;; recenter after jumping with helm
  ;; helm-jump-in-buffer calls helm-semantic-or-imenu
  ;; lambda is necessary to preserve the interactive-ness
  (advice-add 'helm-semantic-or-imenu    :after 'recenter)
  (advice-add 'helm-imenu-in-all-buffers :after '(lambda () (recenter)))

  (with-eval-after-load 'helm
    (define-key helm-map (kbd "<C-return>") 'helm-select-action))

  (with-eval-after-load 'osx-dictionary
    (add-hook 'osx-dictionary-mode-hook 'mw/enable-writing-long-lines))

  (with-eval-after-load 'tide
    (setq tide-tsserver-executable "/usr/local/bin/tsserver"))

  ;; woman doesn't work on mac, set up man instead
  ;; make it immediately open an new window and make it active, also it needs to
  ;; be called interactively
  (setq Man-notify-method 'aggressive)
  (evil-define-motion evil-lookup ()
    (call-interactively evil-lookup-func))
  (setq evil-lookup-func
        (case system-type
          (darwin 'man-follow)
          (otherwise 'woman)))

  ;; boogie set up
  (setq flycheck-dafny-executable "/Users/mwillsey/src/dafny/dafny")
  ;; (setq flycheck-boogie-executable "PATH-TO-BOOGIE")
  ;; (setq flycheck-z3-smt2-executable "z3")
  (setq flycheck-inferior-dafny-executable "/Users/mwillsey/src/dafny/dafny-server")

  ;; browse URLs (including mail) externally
  ;; to force opening a new tab (instead of window) in Safari
  ;; defaults write com.apple.Safari TargetedClicksCreateTabs -bool true
  (setq browse-url-mailto-function  'browse-url-generic
        browse-url-browser-function 'browse-url-default-browser)

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
                                     ("displayline" "displayline -b %n %o %b"))))
      (otherwise
       (setq TeX-view-program-list '(("open" "client open %o")
                                     ("displayline" "client 'displayline -b %n' %o %b")))))

    (setq-default TeX-command-extra-options "-shell-escape")

    (add-to-list 'LaTeX-verbatim-macros-with-delims "lstinline")

    (setq TeX-auto-local  ".auctex.auto"
          TeX-style-local ".auctex.style"))

  (with-eval-after-load 'python
    (setq python-shell-completion-native-enable nil
          python-shell-interpreter "python3"
          python-test-runner 'pytest
          pytest-cmd-flags "-x -s -vvv"))

  (with-eval-after-load 'magit
    ;; don't use use built-in vc anymore
    (setq vc-handled-backends (delq 'Git vc-handled-backends))
    ;; save my files for me
    (setq magit-save-repository-buffers 'dontask
          magit-diff-refine-hunk t))

  (with-eval-after-load 'graphviz-dot-mode
    (setq graphviz-dot-view-command "open %s")
    (spacemacs/set-leader-keys-for-major-mode 'graphviz-dot-mode
      "v" 'graphviz-dot-view))

  (setq gc-cons-threshold (* 800 1024)))
