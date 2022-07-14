;;---------Straight package manager-------
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))

  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;Use-package integration
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;;;--------General Defaults--------
(add-to-list 'load-path "~/.emacs.d/lisp/")

;;Disable backup files
(setq make-backup-files nil)

(setq initial-scratch-message "")

;; Revert Dired and other buffers
(customize-set-variable 'global-auto-revert-non-file-buffers t)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Use "y" and "n" to confirm/negate prompt instead of "yes" and "no"
;; Using `advice' here to make it easy to reverse in custom
;; configurations with `(advice-remove 'yes-or-no-p #'y-or-n-p)'
;;
;; N.B. Emacs 28 has a variable for using short answers, which should
;; be preferred if using that version or higher.
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (advice-add 'yes-or-no-p :override #'y-or-n-p))

;; Do not saves duplicates in kill-ring
(customize-set-variable 'kill-do-not-save-duplicates t)

;; Make scrolling less stuttered
(setq auto-window-vscroll nil)
(customize-set-variable 'fast-but-imprecise-scrolling t)
(customize-set-variable 'scroll-conservatively 101)
(customize-set-variable 'scroll-margin 0)
(customize-set-variable 'scroll-preserve-screen-position t)

;; Better support for files with long lines
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)
(global-so-long-mode 1)

(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;;System variables
(straight-use-package 'exec-path-from-shell)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(when (daemonp)
  (exec-path-from-shell-initialize))


(setq tab-width 4)
;;;-------Evil Mode---------
(straight-use-package 'evil)
;;(straight-use-package 'undo-tree)
(straight-use-package 'evil-collection)
(straight-use-package 'evil-nerd-commenter)

;; Turn on undo-tree globally
;;(global-undo-tree-mode)

;; Set some variables that must be configured before loading the package
(customize-set-variable 'evil-want-integration t)
(customize-set-variable 'evil-want-keybinding nil)
(customize-set-variable 'evil-want-C-i-jump nil)
(customize-set-variable 'evil-respect-visual-line-mode t)
;;(customize-set-variable 'evil-undo-system 'undo-tree)

(setq evil-want-C-u-scroll t)

;; Turn on Evil Nerd Commenter
(evilnc-default-hotkeys)

;; Make C-g revert to normal state
(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

;; Rebind `universal-argument' to 'C-M-u' since 'C-u' now scrolls the buffer
(global-set-key (kbd "C-M-u") 'universal-argument)
(global-set-key (kbd "C-u") #'evil-scroll-up)

;; C-h is backspace in insert state
(define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

;; Use visual line motions even outside of visual-line-mode buffers
(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)

;; Load Evil and enable it globally
(require 'evil)
(evil-mode 1)

;; Make sure some modes start in Emacs state
(dolist (mode '(custom-mode
                eshell-mode
                term-mode))
  (add-to-list 'evil-emacs-state-modes mode))
 
(evil-collection-init)

;;;---------UI Improvements----------
(setq default-frame-alist '((font . "Fira Mono Medium 18")))

(straight-use-package 'gruber-darker-theme)
(straight-use-package 'modus-themes)
(straight-use-package 'all-the-icons)
(straight-use-package 'doom-modeline)
(straight-use-package 'elisp-demos)
(straight-use-package 'helpful)

(require 'modus-themes)
(modus-themes-load-themes)
(modus-themes-load-vivendi)

;; Configure `doom-modeline'
(customize-set-variable 'doom-modeline-height 10)
(customize-set-variable 'doom-modeline-bar-width 6)
(customize-set-variable 'doom-modeline-minor-modes t)
(customize-set-variable 'doom-modeline-buffer-file-name-style 'truncate-except-project)
(doom-modeline-mode 1)

;; Make `describe-*' screens more helpful
(require 'helpful)
(define-key helpful-mode-map [remap revert-buffer] #'helpful-update)
(global-set-key [remap describe-command] #'helpful-command)
(global-set-key [remap describe-function] #'helpful-callable)
(global-set-key [remap describe-key] #'helpful-key)
(global-set-key [remap describe-symbol] #'helpful-symbol)
(global-set-key [remap describe-variable] #'helpful-variable)
(global-set-key (kbd "C-h F") #'helpful-function)
(global-set-key (kbd "C-h K") #'describe-keymap)
;; also add some examples
(require 'elisp-demos)
(advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)

;; add visual pulse when changing focus, like beacon but built-in
;; from from https://karthinks.com/software/batteries-included-with-emacs/
(defun pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))

(dolist (command '(scroll-up-command scroll-down-command
                                     recenter-top-bottom other-window))
  (advice-add command :after #'pulse-line))

;;;-------COMPLETION/SEARCHING------------

(straight-use-package 'cape)
(straight-use-package 'consult)
(straight-use-package 'corfu-doc)
(straight-use-package 'corfu)
(straight-use-package 'embark)
(straight-use-package 'embark-consult)
(straight-use-package 'marginalia)
(straight-use-package 'orderless)
(straight-use-package '( vertico :files (:defaults "extensions/*")
                         :includes (vertico-buffer
                                    vertico-directory
                                    vertico-flat
                                    vertico-indexed
                                    vertico-mouse
                                    vertico-quick
                                    vertico-repeat
                                    vertico-reverse)))

(require 'vertico)

(defun minibuffer-backward-kill (arg)
  "When minibuffer is completing a file name delete up to parent
folder, otherwise delete a word"
  (interactive "p")
  (if minibuffer-completing-file-name
      ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
    (backward-kill-word arg)))

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(with-eval-after-load 'evil
  (define-key vertico-map (kbd "C-j") 'vertico-next)
  (define-key vertico-map (kbd "C-k") 'vertico-previous)
  (define-key vertico-map (kbd "M-h") 'vertico-directory-up))

;; Cycle back to top/bottom result when the edge is reached
(customize-set-variable 'vertico-cycle t)

;; Start Vertico
(vertico-mode 1)

;;; Marginalia
;; Configure Marginalia
(require 'marginalia)
(customize-set-variable 'marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
(marginalia-mode 1)

;; Set some consult bindings
(global-set-key (kbd "C-s") 'consult-line)
(define-key minibuffer-local-map (kbd "C-r") 'consult-history)

(setq completion-in-region-function #'consult-completion-in-region)

;;; Orderless
;; Set up Orderless for better fuzzy matching
(require 'orderless)
(customize-set-variable 'completion-styles '(orderless))
(customize-set-variable 'completion-category-overrides '((file (styles . (partial-completion)))))

;;; Embark
(require 'embark)
(require 'embark-consult)

(global-set-key [remap describe-bindings] #'embark-bindings)
(global-set-key (kbd "C-.") 'embark-act)

;; Use Embark to show bindings in a key prefix with `C-h`
(setq prefix-help-command #'embark-prefix-help-command)

(with-eval-after-load 'embark-consult
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

;;; Corfu
;; Setup corfu for popup like completion
(customize-set-variable 'corfu-cycle t) ; Allows cycling through candidates
(customize-set-variable 'corfu-auto t)  ; Enable auto completion
(customize-set-variable 'corfu-auto-prefix 2) ; Complete with less prefix keys
(customize-set-variable 'corfu-auto-delay 0.0) ; No delay for completion
(customize-set-variable 'corfu-echo-documentation 0.25) ; Echo docs for current completion option

(global-corfu-mode 1)

(add-hook 'corfu-mode-hook #'corfu-doc-mode)
(define-key corfu-map (kbd "M-p") #'corfu-doc-scroll-down)
(define-key corfu-map (kbd "M-n") #'corfu-doc-scroll-up)
(define-key corfu-map (kbd "M-d") #'corfu-doc-toggle)

;;; Cape

;; Setup Cape for better completion-at-point support and more
(require 'cape)

;; Add useful defaults completion sources from cape
(add-to-list 'completion-at-point-functions #'cape-file)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)

;; Silence the pcomplete capf, no errors or messages!
;; Important for corfu
(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

;; Ensure that pcomplete does not write to the buffer
;; and behaves as a pure `completion-at-point-function'.
(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)
(add-hook 'eshell-mode-hook
          (lambda () (setq-local corfu-quit-at-boundary t
				 corfu-quit-no-match t
				 corfu-auto nil)
            (corfu-mode)))

(straight-use-package 'company)

;;;---------WHICH-KEY----------
(straight-use-package 'which-key)
(require 'which-key)
(setq which-key-idle-delay 0.3)
(which-key-mode 1)

;;;---------GENERAL KEYBINDINGS------------
(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer leader-keys
    :keymaps '(normal)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-define-key
   :states 'normal
   "<S-K>" 'lsp-describe-thing-at-point)

  (global-set-key (kbd "C-u") #'evil-scroll-up)
  (global-set-key (kbd "C-M-l") #'lsp-format-buffer)
  (global-set-key (kbd "M-k") #'lsp-describe-thing-at-point)

  (leader-keys
    "." '(find-file-at-point :which-key "Find File"))
  (leader-keys
    "h" '(:ignore t :which-key "Help")
    "hf" '(describe-function :which-key "Function")
    "hv" '(describe-variable :which-key "Variable")
    "hk" '(describe-key :which-key "Key"))
  (leader-keys
    "b" '(:ignore t :which-key "Buffer")
    "bk" '(kill-current-buffer :which-key "Kill buffer")
    "bi" '(ibuffer :which-key "ibuffer"))
  (leader-keys
    "c" '(:ignore t :which-key "Code")
    "cd" '(lsp-find-definition :which-key "Jump to definition")
    "cf" '(lsp-find-references :which-key "Find references")
    "ck" '(lsp-describe-thing-at-point  :which-key "Documentation")
    "cl" '(lsp-format-buffer :which-key "Format buffer")
	"cr" '(lsp-rename :which-key "Rename"))
  (leader-keys
    "e" '(:ignore t :which-key "Eval")
    "ee" '(cider-eval-last-sexp :which-key "Eval last s-exp")
    "eb" '(cider-eval-buffer :which-key "Eval buffer")
    "ea" '(cider-eval-all-files :which-key "Eval all files"))
  (leader-keys
    "n" '(:ignore t :which-key "Notes")
    "nc" '(org-roam-capture :which-key "Create note")
    "ni" '(org-roam-node-insert :which-key "Insert note")
    "nf" '(org-roam-node-find :which-key "Find note"))
  (leader-keys
    "w" '(:ignore t :which-key "Window")
    "wl" '(evil-window-right :which-key "Jump right")
    "wh" '(evil-window-left :which-key "Jump left")
    "wj" '(evil-window-down :which-key "Jump down")
    "wk" '(evil-window-up :which-key "Jump up"))
  (leader-keys
    "j" '(:ignore t :which-key "Janet")
    "jb" '(ijanet-eval-buffer :which-key "Eval buffer")
    "je" '(ijanet-eval-sexp-at-point :which-key "Eval expression"))
  (leader-keys
    "f" '(:ignore t :which-key "File")
    "ff" '(find-file-at-point :which-key "Find file")
    "fs" '(save-buffer :which-key "Save file"))
  (leader-keys
    "v" '(:ignore t :which-key "vterm")
    "vv" '(vterm :which-key "open vterm on other window")
    "vo" '(v-term-other-window :which-key "open vterm in other window"))
  )

(defun generic-eval-buffer ()
  (interactive
   (cider-eval-buffer)))

;;;---------LSP----------------
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (clojure-mode . lsp)
	 (rust-mode . lsp)
	 (c-mode . lsp)
	 (go-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode)

;;Syntax checking
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;;For debugging
(use-package dap-mode)

;;;---------Treemacs-----------
(use-package treemacs
  :ensure t
  :defer t
  :config
(progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                5000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expanf          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

;;;---------RUST---------------
(use-package rustic)
(add-hook 'rust-mode-hook #'smartparens-mode)

;;;---------GO-----------------
(straight-use-package 'go-mode)
(add-hook 'go-mode-hook (lambda () (setq tab-width 4)))
(add-hook 'go-mode-hook (lambda () (company-mode 0)))

;;;---------CLOJURE------------
(straight-use-package 'cider)
(straight-use-package 'clojure-mode)
(require 'cider)
(require 'clojure-mode)
(add-hook 'clojure-mode-hook #'smartparens-mode)

;;;---------JANEGT------------

(straight-use-package
 '(ijanet
   :type git
   :host github
   :repo "serialdev/ijanet-mode"
))

;(require 'janet-mode)
;(add-hook 'janet-mode-hook #'smartparens-mode)


;;;---------MAGIT-------------
(straight-use-package 'magit)
(straight-use-package 'evil-magit)

;;---------SMARTPARENS--------
(straight-use-package 'smartparens)
(require 'smartparens-config)
(add-hook 'lisp-mode-hook #'smartparens-mode)
(add-hook 'elisp-mode-hook #'smartparens-mode)

;;---------ORG--------
;;(straight-use-package 'org-appear)

;; Return or left-click with mouse follows link
;;(customize-set-variable 'org-return-follows-link t)
;;(customize-set-variable 'org-mouse-1-follows-link t)

;; Display links as the description provided
;;(customize-set-variable 'org-descriptive-links t)

;; Hide markup markers
;;(customize-set-variable 'org-hide-emphasis-markers t)

;(add-hook 'org-mode-hook 'org-appear-mode)

(use-package org)

;;---------ORG-ROAM--------
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/org/"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %?"
         :if-new (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n"))))

  (org-roam-db-autosync-mode)

  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

;;;---------VTERM-------------
(use-package vterm
    :ensure t)



;;;---------CUSTOM------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("dad40020beea412623b04507a4c185079bff4dcea20a93d8f8451acb6afc8358" "e13beeb34b932f309fb2c360a04a460821ca99fe58f69e65557d6c1b10ba18c7" default))
 '(ignored-local-variable-values
   '((elisp-lint-indent-specs
      (if-let* . 2)
      (when-let* . 1)
      (let* . defun)
      (nrepl-dbind-response . 2)
      (cider-save-marker . 1)
      (cider-propertize-region . 1)
      (cider-map-repls . 1)
      (cider--jack-in . 1)
      (cider--make-result-overlay . 1)
      (insert-label . defun)
      (insert-align-label . defun)
      (insert-rect . defun)
      (cl-defun . 2)
      (with-parsed-tramp-file-name . 2)
      (thread-first . 0)
      (thread-last . 0))
     (checkdoc-package-keywords-flag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
