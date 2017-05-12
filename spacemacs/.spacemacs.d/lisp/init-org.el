
(message "org loading...")
(require 'org)
(require 'org-agenda)
(message "org loaded!")
(message "org configuring...")

(add-hook 'org-mode-hook 'mw/org-mode-hook)
(defun mw/org-mode-hook ()
  (auto-fill-mode 1))

;; open things on the client side
(case system-type
  (darwin
   (setq org-file-apps
         '((auto-mode . emacs)
           ("\\.x?html?\\'" .  "open %s")
           ("\\.pdf\\'"     .  "open %s"))))
  (otherwise
   (setq org-file-apps
         '((auto-mode . emacs)
           ("\\.x?html?\\'" .  "client open %s")
           ("\\.pdf\\'"     .  "client open %s")))))

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

;; don't open new windows for file links
(add-to-list 'org-link-frame-setup '(file . find-file))

;; dim drawers and stuff, they are too bold as-is
(mw/take-face-attribute 'org-special-keyword 'shadow :foreground)

;; clocking

;; Resume clocking task on clock-in if the clock is open
(org-clock-persistence-insinuate)
(setq org-clock-persist t
      org-clock-persist-query-resume t)

;; clock setup
(setq org-clock-in-resume t
      org-clock-in-switch-to-state nil
      org-clock-into-drawer t
      org-clock-out-remove-zero-time-clocks t
      org-clock-out-when-done t
      org-clock-report-include-clocking-task t
      org-clock-auto-clock-resolution 'when-no-clock-is-running
      org-clock-mode-line-total 'today)

(spacemacs/set-leader-keys
  "aog" 'org-clock-goto
  "aoI" 'org-clock-in)
(spacemacs/toggle-mode-line-org-clock-on)

;; put everything into a drawer
(setq org-log-done nil
      org-log-into-drawer t
      org-agenda-log-mode-items '(closed state clock))

(setq org-enforce-todo-dependencies t)


(setq org-todo-keywords '((sequence
                           "TODO(t)" "NEXT(n!)" "HOLD(h@/!)"
                           "|"
                           "DONE(d!)" "CANCELED(c@/!)")
                          (type "MEET(m)"))
      org-todo-keyword-faces '(("TODO" . "red1")
                               ("NEXT" . "magenta1")
                               ("DONE" . "green4")
                               ("HOLD" . "gold3")
                               ("CANCELED" . "blue3")))

(setq org-capture-templates
      '(("t" "todo"
         entry (file org-default-notes-file)
         "* TODO %?\n%U")
        ("m" "meeting"
         entry (file org-default-notes-file)
         "* MEET %?\n%U")
        ("w" "website"
         entry (file org-default-notes-file)
         "* TODO %(org-mac-safari-get-frontmost-url)\n%U"
         :immediate-finish t)
        (" " "blank"
         entry (file org-default-notes-file)
         "* %?\n%U")))

(add-hook 'org-capture-mode-hook 'evil-insert-state)

;; agenda files are either at the top level or in those folders
(setq org-agenda-files
      (cons org-directory
            (mapcar 'mw/dir (list "work/" "personal/"))))

;; (add-hook 'org-agenda-mode-hook 'mw/org-agenda-mode-hook)
;; (defun mw/org-agenda-mode-hook ()
;;   (olivetti-mode     1)
;;   (visual-line-mode -1)
;;   (olivetti-set-width 85))

;; clean 3 day view, only show the time grid on a single day
;; show preceding weekends for weekly views
(setq org-agenda-span 3
      org-agenda-start-on-weekday 6
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

;; spacemacs niceties
(spacemacs/set-leader-keys-for-major-mode 'org-mode
  "r" 'org-refile)
(spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
  "r" 'org-agenda-refile)
(add-to-list 'spacemacs-useful-buffers-regexp "\\*Org Agenda\\*")


;; agenda should make sure to include things that need to be refiled
(setq org-agenda-custom-commands
      '(("a" "Agenda"
         ((agenda "" nil)
          (tags "refile"
                ((org-agenda-overriding-header "Refile")
                 (org-tags-match-list-sublevels t)
                 (org-agenda-start-with-log-mode nil)
                 (org-agenda-start-with-clockreport-mode nil))))
         nil
         ("~/Dropbox/org/agenda.html"))
        ("w" "Weekly Review"
         agenda ""
         (;; only show work stuff
          (org-agenda-files (cons org-directory
                                  (mapcar 'mw/dir (list "work/"))))
          (org-agenda-span 'week)
          (org-agenda-entry-types '(:sexp :timestamp :deadline))
          (org-agenda-start-with-log-mode t)
          (org-agenda-start-with-clockreport-mode t)))))

;; bind my custom agendas
(defun mw/org-agenda () (interactive) (org-agenda nil "a"))
(defun mw/org-review () (interactive) (org-agenda nil "w"))
(spacemacs/set-leader-keys
  "aoa" 'mw/org-agenda
  "aow" 'mw/org-review)

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

(setq reftex-cite-punctuation '(", " " and " " {\\it et al.}"))
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
                   ":END:\n") "\n"))


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
        (:dbuser . "mwillsey")))

(message "org configured!")
(provide 'init-org)
