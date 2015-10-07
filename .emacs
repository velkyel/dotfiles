
(require 'package)
(package-initialize)

(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))

(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)
(require 'diminish)
(require 'bind-key)

(setq compilation-ask-about-save nil
      compilation-scroll-output 'first-error
      next-line-add-newlines nil
      inhibit-startup-message t
      initial-scratch-message nil
      load-prefer-newer t
      require-final-newline t
      redisplay-dont-pause t
      column-number-mode t
      make-backup-files nil
      delete-auto-save-files t
      uniquify-buffer-name-style 'forward
      ediff-window-setup-function 'ediff-setup-windows-plain
      vc-diff-switches "-u")

(if window-system
    (progn
      (tool-bar-mode -1)
      (tooltip-mode -1)
      (scroll-bar-mode -1)
      (fringe-mode 2)))

(defun kelly? ()
  (or (string= system-name "typhoon.autokelly.local")
      (string= system-name "idev02.autokelly.local")
      (string= system-name "idev03.autokelly.local")))

(setq gc-cons-threshold 20000000)
;; (setq compilation-skip-threshold 2)

(defalias 'after 'with-eval-after-load)
(delete-selection-mode t)
(show-paren-mode 1)
(transient-mark-mode t)
(menu-bar-mode -1)
(which-function-mode)
;; (iswitchb-mode t)   ;; substring buffer switch
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default py-indent-offset 4)
;; (setq-default abbrev-mode t)
(winner-mode t)   ;; C-c <left|right>
(set-language-environment "czech")
(setq default-input-method "czech-qwerty")
(if (kelly?)
    (progn
      (prefer-coding-system 'iso-latin-2-unix)
      (set-terminal-coding-system 'iso-latin-2-unix)
      (set-keyboard-coding-system 'iso-latin-2-unix))
  (progn
    (prefer-coding-system 'utf-8)
    (set-terminal-coding-system 'utf-8)
    (set-keyboard-coding-system 'utf-8)))

(defalias 'yes-or-no-p 'y-or-n-p)

;(setq-default line-spacing nil)
(setq global-auto-revert-non-file-buffers t)
;; (setq auto-revert-verbose nil)
(global-auto-revert-mode 1)

;; ; check if we're on OSX
(when (featurep 'ns-win)
  ;; (setq mac-command-modifier 'meta)
  ;; (setq mac-option-modifier nil)
  (setq mac-option-modifier 'nil)
  (setq mac-command-modifier 'meta)
  (setq ns-function-modifier 'hyper)
  (set-default-font "Inconsolata 17"))

(when (and window-system (equal system-type 'gnu/linux))
  (set-default-font "Inconsolata 13"))
;; (setq x-alt-keysym 'meta)

(use-package json-mode)
(use-package lua-mode)
(use-package rust-mode)
(use-package swift-mode)
(use-package processing-mode)

(use-package diffview)

(use-package ag)
(use-package helm-ag)

(use-package projectile
  :diminish projectile-mode
  :bind ("C-c C-f" . projectile-find-file)
  :init
  (projectile-global-mode)
  :bind ("M-g" . helm-projectile-ag))

(use-package helm
  :defer t
  :init
  (setq helm-mode-fuzzy-match t
        helm-competion-in-region-fuzzy-match t)
  (helm-push-mark-mode 1)
  (define-key global-map [remap list-buffers] 'helm-buffers-list)
  ;; (define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
  :bind (("M-x" . helm-M-x)
         ("C-x b" . helm-buffers-list)
         ("C-." . helm-imenu-in-all-buffers)
         ("M-y" . helm-show-kill-ring)
         ("C-c <SPC>" . helm-all-mark-rings)))

(use-package helm-descbinds
  :config (helm-descbinds-mode))

;; (use-package helm-swoop
;;   :bind ("C-s" . helm-swoop))

(use-package helm-projectile
  :init
  (setq projectile-completion-system 'helm)
  (helm-projectile-on))

(use-package eshell
  :defer t
  :config
  (setq eshell-command-aliases-list (append '(("l" "ls -lcrt")
                                              ("d" "dired $1")
                                              ("ff" "find-file $1"))
                                            eshell-command-aliases-list))
  :init
  (defun visit-term-buffer ()
    "Create or visit a terminal buffer."
    (interactive)
    (if (not (get-buffer "*eshell*"))
        (progn
          (split-window-sensibly (selected-window))
          (other-window 1)
          (eshell))
      (switch-to-buffer-other-window "*eshell*")))
  (bind-key "C-c t" 'visit-term-buffer))

(use-package avy
  :defer t
  :bind (("C-;" . avy-goto-word-or-subword-1))
         ;;("M-g l" . avy-goto-line))
  :init (after 'isearch (define-key isearch-mode-map (kbd "C-;") 'avy-isearch)))

(use-package saveplace
  :init (setq-default save-place t))

(use-package vc-darcs
  :init
  (setq vc-disable-async-diff nil)                ;; hotfix
  (add-to-list 'vc-handled-backends 'DARCS t)
  (autoload 'vc-darcs-find-file-hook "vc-darcs")
  (add-hook 'find-file-hooks 'vc-darcs-find-file-hook))

(use-package ninja-mode)

(use-package clojure-mode
  :pin melpa-stable)

(use-package pixie-mode
  :init (add-hook 'pixie-mode-hook #'inf-clojure-minor-mode))

(use-package browse-kill-ring
  :init
  (define-key (current-global-map) [remap yank-pop] 'browse-kill-ring)   ;; remap yank-pop
  (setq browse-kill-ring-replace-yank t))

(use-package whitespace
  :diminish whitespace-mode
  :config
  (setq whitespace-line-column 100)             ;; limit line length
  (setq whitespace-style '(face trailing newline))
  :init
  (add-hook 'prog-mode-hook 'whitespace-mode))

(use-package shrink-whitespace
  :bind ("M-\\" . shrink-whitespace))

(use-package elpy
  :defer t
  :config
  (remove-hook 'elpy-modules 'elpy-module-yasnippet)
  (when (or (equal system-type 'darwin) (kelly?))
    (remove-hook 'elpy-modules 'elpy-module-flymake)))
  :init
  (elpy-enable)

(use-package expand-region
  :defer t
  :bind ("C-=" . er/expand-region))

(use-package rainbow-mode
  :defer t
  :diminish rainbow-mode
  :init (add-hook 'emacs-lisp-mode-hook 'rainbow-mode))

(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(FIXME\\|TODO\\):" 1 font-lock-preprocessor-face prepend)))))

(if (kelly?)
    (set-background-color "gray90")
  (use-package flatui-theme
    :init (load-theme 'flatui t)))

(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

(use-package google-translate
  :defer t
  :init (require 'google-translate-default-ui))

(use-package mwim
  :bind ("C-a" . mwim-beginning-of-code-or-line))
;; ("C-e" . mwim-end-of-code-or-line)

(add-to-list 'auto-mode-alist '("\\.mm$" . objc-mode))

(use-package glsl-mode
  :defer t
  :mode ("\\.\\(glsl\\|vert\\|frag\\|vsh\\|fsh\\|usf\\)\\'" . glsl-mode))     ;; usf = unreal engine

(use-package smtpmail
  :if (not (kelly?))
  :defer t
  :config
  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-auth-credentials "~/.authinfo"
        ;;smtpmail-stream-type 'ssl
        starttls-use-gnutls t
        smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
        smtpmail-auth-credentials '(("smtp.gmail.com" 587 "capak44@gmail.com" nil))
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587))

(when (not (kelly?))
  (when (equal system-type 'gnu/linux)
    (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e"))
  (when (equal system-type 'darwin)
    (setq mu4e-mu-binary "/usr/local/bin/mu")
    (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e"))
  (when (equal system-type 'gnu/linux)
    (setq mu4e-mu-binary "/usr/bin/mu"))
  (require 'mu4e)
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))
  (setq epa-file-cache-passphrase-for-symmetric-encryption t
        mu4e-maildir "~/Maildir"
        mu4e-drafts-folder "/[Gmail].Drafts"
        mu4e-sent-folder   "/[Gmail].Sent Mail"
        mu4e-trash-folder  "/[Gmail].Trash"
        mu4e-maildir-shortcuts
        '( ("/INBOX"             . ?i)
           ("/[Gmail].Sent Mail" . ?s)
           ("/[Gmail].Trash"     . ?t)
           ("/[Gmail].All Mail"  . ?a))
        mu4e-bookmarks '(((concat "flag:unread"
                                  " AND NOT flag:trashed"
                                  " AND NOT maildir:/[Gmail].Trash"
                                  " AND NOT maildir:/[Gmail].Spam")
                          "Unread messages"      ?u)
                         ("date:today..now"                  "Today's messages"     ?t)
                         ("date:7d..now"                     "Last 7 days"          ?w)
                         ("mime:image/*"                     "Messages with images" ?p)
                         ("size:2M..500M"                    "Big messages"         ?b))
        mu4e-get-mail-command "offlineimap -q"
        mu4e-update-interval nil
        mu4e-view-show-images t
        mu4e-html2text-command "w3m -T text/html"
        mu4e-headers-skip-duplicates t
        user-mail-address "capak@inputwish.com"
        user-full-name  "Libor Čapák"
        mail-signature nil
        mail-signature-file nil
        message-signature nil
        mu4e-compose-signature nil
        mu4e-compose-signature-auto-include nil
        mu4e-sent-message-behaviour 'delete
        mu4e-hide-index-messages t
        mu4e-view-show-addresses t
        mu4e-date-format-long "%d.%m.%Y"
        mu4e-headers-date-format "%d.%m.%y"
        mu4e-confirm-quit nil
        message-kill-buffer-on-exit t)
  ;; (setq mu4e-use-fancy-chars t)
  (set-face-attribute 'mu4e-unread-face nil
                      :inherit font-lock-preprocessor-face
                      :bold t)
  (defun mu4e-msgv-action-view-in-browser (msg)
    "View the body of the message in a web browser."
    (interactive)
    (let ((html (mu4e-msg-field (mu4e-message-at-point t) :body-html))
          (tmpfile (format "%s/%d.html" temporary-file-directory (random))))
      (unless html (error "No html part for this message"))
      (with-temp-file tmpfile
        (insert
         "<html>"
         "<head><meta http-equiv=\"content-type\""
         "content=\"text/html;charset=UTF-8\">"
         html))
      (browse-url (concat "file://" tmpfile))))
  (add-to-list 'mu4e-view-actions
               '("View in browser" . mu4e-msgv-action-view-in-browser) t)
  (defun run ()
    (interactive)
    (mu4e)
    (mu4e-update-mail-and-index nil))
  (global-set-key (kbd "C-c m") 'run))

(defun my-c-mode-font-lock-if0 (limit)
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (let ((depth 0) str start start-depth)
        (while (re-search-forward "^\\s-*#\\s-*\\(if\\|else\\|endif\\)" limit 'move)
          (setq str (match-string 1))
          (if (string= str "if")
              (progn
                (setq depth (1+ depth))
                (when (and (null start) (looking-at "\\s-+0"))
                  (setq start (match-end 0)
                        start-depth depth)))
            (when (and start (= depth start-depth))
              (c-put-font-lock-face start (match-beginning 0) 'font-lock-comment-face)
              (setq start nil))
            (when (string= str "endif")
              (setq depth (1- depth)))))
        (when (and start (> depth 0))
          (c-put-font-lock-face start (point) 'font-lock-comment-face)))))
  nil)
(defun my-c-mode-common-hook ()
  (progn
    (font-lock-add-keywords
     nil
     '((my-c-mode-font-lock-if0 (0 font-lock-comment-face prepend))) 'add-to-end)
    (setq fill-column 100)))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(use-package nyan-mode
  :if (not (kelly?))
  :init
  (nyan-mode 1)
  (setq nyan-bar-length 16
        naya-wavy-trail t))

(use-package cider
  :if (not (kelly?))
  :pin melpa-stable
  :defer t
  :config
  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
  ;; (setq cider-auto-mode nil)
  ;; (setq nrepl-log-messages nil)
  ;; (setq nrepl-hide-special-buffers t)
  (setq cider-repl-use-pretty-printing t)
  (setq cider-prompt-save-file-on-load nil))

(use-package racket-mode
  :if (not (kelly?))
  :defer t
  :config
  (add-hook 'racket-mode-hook          ;; same as C-c C-k
            (lambda ()
              (define-key racket-mode-map (kbd "C-c r") 'racket-run))))

(when (not (kelly?))
  (setq compile-command "scons"))

;; isearch
;;  :diminish isearch-mode
(setq search-highlight t
      isearch-allow-scroll t)

(diminish 'abbrev-mode)
(diminish 'isearch-mode)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(define-key function-key-map "\e[$" (kbd "C-$"))
(define-key function-key-map "\e[%" (kbd "C-%"))
(define-key function-key-map "\e[," (kbd "C-,"))
(define-key function-key-map "\e[;" (kbd "C-;"))
(define-key function-key-map "\e[=" (kbd "C-="))
(define-key function-key-map "\e[." (kbd "C-."))

(define-key global-map (kbd "RET") 'newline-and-indent)
;; (define-key c-mode-map (kbd "TAB") 'company-indent-or-complete-common)
;; (define-key c++-mode-map (kbd "TAB") 'company-indent-or-complete-common)
(global-set-key (kbd "M-r") 'recompile)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-G") 'grep)

(setq font-lock-maximum-decoration '((racket-mode . t) (t . 1)))

;; (add-hook 'c++-mode-hook 'irony-mode)
;; (add-hook 'c-mode-hook 'irony-mode)
;; (add-hook 'objc-mode-hook 'irony-mode)

;; (defun my-irony-mode-hook ()
;;   (define-key irony-mode-map [remap completion-at-point]
;;     'irony-completion-at-point-async)
;;   (define-key irony-mode-map [remap complete-symbol]
;;     'irony-completion-at-point-async))
;; (add-hook 'irony-mode-hook 'my-irony-mode-hook)
;; (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; (require 'company)
;; (require 'company-irony)
;; (setq company-backends (delete 'company-semantic company-backends)
;;       company-global-modes '(not term-mode)
;;       company-transformers '(company-sort-by-occurrence)
;;       company-minimum-prefix-length 2
;;       company-selection-wrap-around t
;;       company-show-numbers t
;;       company-require-match nil
;;       company-dabbrev-downcase nil)
;; (add-to-list 'company-backends 'company-irony)
;; ;; (setq company-idle-delay 0.1)
;; (add-hook 'prog-mode-hook (lambda () (company-mode 1)))
