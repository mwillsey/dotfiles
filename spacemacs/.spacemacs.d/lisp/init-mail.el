
(add-to-load-path "/usr/local/share/emacs/site-lisp/notmuch")
(require 'notmuch)
(require 'org-notmuch)

(add-to-list 'spacemacs-useful-buffers-regexp "\\*notmuch.*\\*")

(setq notmuch-archive-tags '("-inbox" "-unread")
      notmuch-search-result-format
      '(("date"    . "%12s ")
        ("count"   . "%-7s ")
        ("tags"    . "%-11s ")
        ("authors" . "%-20s ")
        ("subject" . "%s"))
      notmuch-search-line-faces
      '(("unread" . notmuch-search-unread-face))
      notmuch-tag-formats
      '(("unread")  ;; use the highlight instead
        ("replied") ;; sent is better
        ("inbox"      "in" (propertize tag 'face '(:foreground "blue")))
        ("flagged"    "fl" (propertize tag 'face 'notmuch-tag-flagged))
        ("attachment" "at" (propertize tag 'face '(:foreground "green4")))
        ("sent"       "sn" (propertize tag 'face '(:foreground "yellow4")))
        ))

(defun mw/notmuch-search-trash-thread (&optional beg end)
  "The trash version of `notmuch-search-archive-thread'"
  (interactive (notmuch-search-interactive-region))
  (notmuch-search-tag '("+trash" "-inbox") beg end)
  (when (eq beg end) (notmuch-search-next-thread)))

(setq notmuch-search-oldest-first nil)

;; hijack mu4e-alert-segment because it's already in spaceline
(spaceline-define-segment mu4e-alert-segment
  (when active
    (let ((cmd (process-lines notmuch-command "count"
                              "is:inbox and is:unread")))
      (format "[%d]" (string-to-number (car cmd))))))
(spaceline-compile)


(spacemacs/declare-prefix "am" "mail")

(setq notmuch-fcc-dirs '(("mwillsey@gmail.com" . "mw/sent")
                         (".*@mwillsey.com"    . "mw/sent")
                         ("mwillsey@cs.washington.edu" . "cse/sent")))

;; use msmtp and let it figure out the account
;; don't use "-f envelop-from"
(setq sendmail-program (executable-find "msmtp")
      message-send-mail-function 'message-send-mail-with-sendmail
      message-sendmail-f-is-evil t
      message-sendmail-extra-arguments '("--read-envelope-from"))


;; below from
;; https://github.com/mullr/spacemacs-custom/blob/master/layers/notmuch/packages.el
;; for now
(defun notmuch/search ()
  (interactive)
  (require 'notmuch)
  (notmuch-search))

(defun notmuch/tree ()
  (interactive)
  (require 'notmuch)
  (notmuch-tree))

(defun notmuch/jump-search ()
  (interactive)
  ;; (require 'notmuch)
  (notmuch-jump-search)
  (bind-map-change-major-mode-after-body-hook))

(defun notmuch/new-mail ()
  (interactive)
  (require 'notmuch)
  (notmuch-mua-new-mail))

(defun notmuch/inbox ()
  (interactive)
  (require 'notmuch)
  (notmuch-search "is:inbox")
  (bind-map-change-major-mode-after-body-hook))

(defun notmuch/tree-show-message ()
  (interactive)
  (notmuch-tree-show-message-in)
  (select-window notmuch-tree-message-window))


;; use-package :init
(spacemacs/set-leader-keys
  "amn" 'notmuch/new-mail
  "amm" 'notmuch/jump-search
  "ami" 'notmuch/inbox)

(spacemacs/set-leader-keys-for-major-mode 'notmuch-search-mode
  "t+" 'notmuch-search-add-tag)

(spacemacs/set-leader-keys-for-major-mode 'notmuch-tree-mode
  "tt" 'notmuch-tree-tag-thread
  "t+" 'notmuch-tree-add-tag
  "t-" 'notmuch-tree-remove-tag
  "r" 'notmuch-tree-refresh-view
  ;; "d" 'notmuch-tree-archive-message-then-next
  ;; "A" 'notmuch-tree-archive-thread
  "g" 'notmuch-poll-and-refresh-this-buffer
  "s" 'notmuch-search-from-tree-current-query
  "c" 'notmuch-show-stash-map
  "m" 'notmuch-mua-new-mail
  "w" 'notmuch-show-save-attachments)

(spacemacs/set-leader-keys-for-minor-mode 'notmuch-message-mode
  "," 'notmuch-mua-send-and-exit
  "k" 'message-kill-buffer)

(evilified-state-evilify-map notmuch-search-mode-map
  :mode notmuch-search-mode
  :bindings
  ;; (kbd "q") 'notmuch-bury-or-kill-this-buffer
  (kbd "d") 'mw/notmuch-search-trash-thread
  (kbd "r") 'notmuch-search-reply-to-thread
  (kbd "R") 'notmuch-search-reply-to-thread-sender
  )

(evilified-state-evilify-map notmuch-show-mode-map
  :mode notmuch-show-mode)

(evilified-state-evilify-map notmuch-tree-mode-map
  :mode notmuch-tree-mode
  :bindings
  (kbd "q") 'notmuch-bury-or-kill-this-buffer
  (kbd "?") 'notmuch-help
  (kbd "RET") 'notmuch/tree-show-message
  (kbd "}") 'notmuch-tree-scroll-or-next
  (kbd "{") 'notmuch-tree-scroll-message-window-back)
