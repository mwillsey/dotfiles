
(require 'mu4e)
(setq mu4e-maildir "~/mail"
      mu4e-get-mail-command "mbsync mw & mbsync cse & wait"
      mu4e-update-interval (* 60 5))

(spacemacs/set-leader-keys "aM" nil) ;; disable old shortcut
(spacemacs/set-leader-keys "am" 'mu4e)

;; This change is required when using mbsync on the backend; otherwise moving
;; messages will go awry with messages like "Maildir error: duplicate UID 1". See
;; http://tiborsimko.org/mbsync-duplicate-uid.html
;; http://pragmaticemacs.com/emacs/fixing-duplicate-uid-errors-when-using-mbsync-and-mu4e/
;; on what the problem is and how to fix it.
(setq mu4e-change-filenames-when-moving t)

;; show related messages in searches, but not duplicate ones
(setq mu4e-headers-skip-duplicates t
      mu4e-headers-include-related nil)

(setq mu4e-use-fancy-chars nil)

;; (add-hook 'mu4e-main-mode-hook 'mu4e-alert-update-mail-count-modeline)


;; NOTE deleting messages doesn't really work. It moves the inbox *copy* to
;; the trash, but leaves a copy in all. Gmail don't really delete until it's in
;; the trash *only*.

(setq mu4e-contexts (list
  (make-mu4e-context
    :name "mw"
    :enter-func (lambda () (mu4e-message "Entering personal context"))
    :leave-func (lambda () (mu4e-message "Leaving personal context"))
    :match-func (lambda (msg)
                  (when msg
                    (string-prefix-p "/mw/" (mu4e-message-field msg :maildir))))
    :vars `(( user-mail-address  . "mwillsey@gmail.com"  )
            ( user-full-name     . "Max Willsey" )
            ( mu4e-sent-folder   . "/mw/sent"   )
            ( mu4e-trash-folder  . "/mw/trash"  )
            ( mu4e-refile-folder . "/mw/all"    )
            ( mu4e-drafts-folder . "/mw/drafts" )))
  (make-mu4e-context
    :name "cse"
    :enter-func (lambda () (mu4e-message "Entering UW CSE context"))
    :leave-func (lambda () (mu4e-message "Leaving UW CSE context"))
    :match-func (lambda (msg)
                  (when msg
                    (string-prefix-p "/cse/" (mu4e-message-field msg :maildir))))
    :vars `(( user-mail-address  . "mwillsey@cs.washington.edu"  )
            ( user-full-name     . "Max Willsey" )
            ( mu4e-sent-folder   . "/cse/sent"   )
            ( mu4e-trash-folder  . "/cse/trash"  )
            ( mu4e-refile-folder . "/cse/all"    )
            ( mu4e-drafts-folder . "/cse/drafts" )))))

; Don't save messages, gmail does this for you.
(setq mu4e-sent-messages-behavior 'delete)

;; Setup notifications. The https://github.com/jwiegley/alert package should "do
;; the right thing" and enable OS-native notifications (if `terminal-notifier'
;; is installed on macOS). Also show number of unread messages in the mode line.
(setq mu4e-enable-mode-line t
      mu4e-enable-notifications t
      mu4e-alert-interesting-mail-query
      "(maildir:/mw/inbox OR maildir:/cse/inbox) AND flag:unread")
(require 'mu4e-alert)
(mu4e-alert-enable-notifications)
(mu4e-alert-enable-mode-line-display)

;; `mu4e' provides bookmarks, basically saved searches.
;; https://www.djcbsoftware.nl/code/mu/mu4e/Bookmarks.html which
(setq mu4e-bookmarks (list
  (make-mu4e-bookmark
    :name "Inboxes"
    :query "maildir:/mw/inbox OR maildir:/cse/inbox"
    :key ?i)
  (make-mu4e-bookmark
    :name  "Unread"
    :query "flag:unread AND NOT flag:trashed"
    :key ?u)
  (make-mu4e-bookmark
    :name "Flagged"
    :query "flag:flagged"
    :key ?f)
  (make-mu4e-bookmark
    :name "Today"
    :query "date:today..now"
    :key ?t)
  (make-mu4e-bookmark
    :name "This week"
    :query "date:7d..now"
    :key ?w)))

;; I don't want to wait to quit or have message buffers lying around.
(setq mu4e-confirm-quit nil
      message-kill-buffer-on-exit t)

;;; Viewing mail
(setq mu4e-view-show-addresses t)

;;; Sending mail

;; use msmtp and let it figure out the account
;; don't use "-f envelop-from"
(setq sendmail-program (executable-find "msmtp")
      message-send-mail-function 'message-send-mail-with-sendmail
      message-sendmail-f-is-evil t
      message-sendmail-extra-arguments '("--read-envelope-from"))
