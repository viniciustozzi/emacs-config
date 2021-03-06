(setq user-full-name "Vinicius Tozzi"
      user-mail-address "viniciustozzi@pm.me")

(map! :leader
      (:prefix-map ("l" . "LSP")
        :desc "Format code" "f" #'lsp-format-buffer
        :desc "Find reference" "r" #'lsp-find-references))

(global-set-key (kbd "C-M-l") 'lsp-format-buffer)
(global-set-key (kbd "M-<f7>") 'lsp-find-references)
(global-set-key (kbd "M-7") 'lsp-find-references)

(map! :leader
      (:prefix-map ("t" . "Tab Mode")
        :desc "New Tab" "n" #'tab-bar-new-tab
        :desc "Search Tab By Name" "SPC" #'tab-bar-select-tab-by-name
        :desc "Switch Tab" "s" #'tab-bar-switch-to-next-tab))

;(setq doom-font (font-spec :family "Fira Mono" :size 16))
;(setq doom-theme 'doom-gruvbox)
;(setq doom-theme 'doom-dracula)
;(setq doom-theme 'doom-Iosvkem)
;(setq doom-theme 'gruber-darker)
;(setq tao-theme-use-sepia nil)

(beacon-mode 1)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(setq org-agenda-files '("~/org/roam/"))
(setq org-roam-index-file "~/org/index.org")

;;Org templates
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/roam/inbox.org" "Tasks")
         "* TODO %? %i")
        ("T" "Todo Tagged" entry (file+headline "~/org/roam/inbox.org" "Tasks")
         "* TODO %? %i %^g")
        ("j" "Journal entry" plain (function org-journal-find-location)
                               "** %(format-time-string org-journal-time-format)%^{Title}\n%i%?"
                               :jump-to-captured t :immediate-finish t)))

;Org journal
(setq org-journal-dir "~/org/roam/journal")

(defun org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  (unless (eq org-journal-file-type 'daily)
    (org-narrow-to-subtree))
  (goto-char (point-max)))

(setq org-journal-enable-agenda-integration t)

(setq shell-file-name "/bin/bash")

(setq display-line-numbers-type 'visual)

;(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
;(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
;(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
;(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

(use-package-hook! evil
  :pre-init
  (setq evil-respect-visual-line-mode t) ;; sane j and k behavior
  t)

; Make horizontal movement cross lines
(setq-default evil-cross-lines t)

(use-package ob-translate)

;(require 'google-translate)
;(require 'google-translate-default-ui)
;(require 'google-translate-smooth-ui)
;(global-set-key "\C-ct" 'google-translate-smooth-translate)
;(global-set-key "\C-cy" 'google-translate-at-point)
;(setq google-translate-translation-directions-alist '(("de" . "en"), ("en" "de")))

;(global-set-key "\C-ct" 'google-translate-at-point)
;(global-set-key "\C-cT" 'google-translate-query-translate)

(setq go-translate-local-language "de")
(setq go-translate-target-language "en")
(global-set-key "\C-ct" 'go-translate)
(global-set-key "\C-cy" 'go-translate-popup)
(setq go-translate-token-current (cons 430675 2721866130))

(require 'org)
(require 'ob-clojure)
(setq org-babel-clojure-backend 'cider)
(require 'cider)

(require 'emms-setup)
(emms-all)
(emms-default-players)
;; Set music directory
(setq emms-source-file-default-directory "~/music")
;; Set app for for retrieving meta-data.
(setq emms-info-functions '(emms-info-exiftool))
;; Looks for album cover arts
(setq emms-browser-covers 'emms-browser-cache-thumbnail-async)

(setq elfeed-feeds
  '("https://hnrss.org/frontpage"
    "https://fedoramagazine.org/feed/"))

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(when (daemonp)
  (exec-path-from-shell-initialize))

(setq user-mail-address "viniciustozzi@protonmail.com"
      user-full-name  "Vinicius Vieira Tozzi"
      mu4e-get-mail-command "mbsync -c ~/.mbsyncrc -a"
      mu4e-update-interval  300
      mu4e-main-buffer-hide-personal-addresses t
      message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "localhost"
      smtpmail-stream-type 'starttls
      smtpmail-auth-credentials "gpg2 --quiet --decrypt ~/.mbsync-pw-mailbox.gpg"
      smtpmail-smtp-service 1025
      mu4e-sent-folder "/.mail/Sent"
      mu4e-drafts-folder "/.mail/Drafts"
      mu4e-trash-folder "/.mail/Trash"
      mu4e-maildir-shortcuts
      '(("/.mail/Inbox"      . ?i)
        ("/.mail/Sent" . ?s)
        ("/.mail/Drafts"     . ?d)
        ("/.mail/Trash"      . ?t)))

(defun efs/exwm-update-class()
  (exwm-workspace-rename-buffer exwm-class-name))

(setq exwm-workspace-number 5)

 ;; When window "class" updates, use it to set the buffer name
(add-hook 'exwm-update-class-hook #'efs/exwm-update-class)

  ;; Rebind CapsLock to Ctrl
(start-process-shell-command "xmodmap" nil "xmodmap ~/.emacs.d/exwm/Xmodmap")

  ;; Set the screen resolution (update this to be the correct resolution for your screen!)
(require 'exwm-randr)
(exwm-randr-enable)
  ;; (start-process-shell-command "xrandr" nil "xrandr --output Virtual-1 --primary --mode 2048x1152 --pos 0x0 --rotate normal")

  ;; Load the system tray before exwm-init
(require 'exwm-systemtray)
(exwm-systemtray-enable)

  ;; These keys should always pass through to Emacs
(setq exwm-input-prefix-keys
  '(?\C-x
    ?\C-u
    ?\C-h
    ?\M-x
    ?\M-`
    ?\M-&
    ?\M-:
    ?\C-\M-j  ;; Buffer list
    ?\C-\ ))  ;; Ctrl+Space

  ;; Ctrl+Q will enable the next key to be sent directly
(define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; Set up global key bindings.  These always work, no matter the input state!
  ;; Keep in mind that changing this list after EXWM initializes has no effect.
(setq exwm-input-global-keys
      `(
          ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
        ([?\s-r] . exwm-reset)

          ;; Move between windows

        ([s-right] . windmove-right)
        ([s-up] . windmove-up)
        ([s-down] . windmove-down)

          ;; Launch applications via shell command
        ([?\s-&] . (lambda (command)
                (interactive (list (read-shell-command "$ ")))
                (start-process-shell-command command nil command)))

          ;; Switch workspace
        ([?\s-w] . exwm-workspace-switch)
        ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))

          ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
        ,@(mapcar (lambda (i)
                `(,(kbd (format "s-%d" i)) .
                (lambda ()

                        (exwm-workspace-switch-create ,i))))
                (number-sequence 0 9))))
