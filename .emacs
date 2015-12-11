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
;; (setq use-package-verbose t)
;; (setq use-package-minimum-reported-time 0.02)
(require 'diminish)
(require 'bind-key)

(setq compilation-ask-about-save nil
      compilation-always-kill t
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
      imenu-auto-rescan t
      echo-keystrokes 0.1
      use-dialog-box nil
      uniquify-buffer-name-style 'forward
      ediff-window-setup-function 'ediff-setup-windows-plain
      font-lock-maximum-decoration '((racket-mode . t) (t . 1))
      vc-diff-switches "-u"
      search-highlight t
      isearch-allow-scroll t)

(if window-system
    (progn
      (tool-bar-mode -1)
      (tooltip-mode -1)
      (scroll-bar-mode -1)
      (fringe-mode 2)))

(defun kelly? ()
  (or (string= system-name "typhoon.autokelly.local")
      (string= system-name "idev02.autokelly.local")
      (string= system-name "idev03")
      (string= system-name "idev03.autokelly.local")))

(setq gc-cons-threshold 20000000)
;; (setq compilation-skip-threshold 2)

(defalias 'after 'with-eval-after-load)
(add-hook 'focus-out-hook (lambda () (save-some-buffers t)))
(delete-selection-mode t)
(show-paren-mode 1)
;; (setq show-paren-style 'expression)
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
      (prefer-coding-system 'utf-8)
      (set-terminal-coding-system 'iso-latin-2-unix)
      (set-keyboard-coding-system 'iso-latin-2-unix))
  (progn
    (prefer-coding-system 'utf-8)
    (set-terminal-coding-system 'utf-8)
    (set-keyboard-coding-system 'utf-8)))

(defalias 'yes-or-no-p 'y-or-n-p)

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

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
  (set-default-font "Menlo 15"))

(when (and window-system (equal system-type 'gnu/linux))
  ;; (set-default-font "Inconsolata 13"))
  (set-default-font "DejaVu Sans Mono 11"))
;; (setq x-alt-keysym 'meta)

(use-package json-mode :defer t)
(use-package lua-mode :defer t)
(use-package haskell-mode :defer t)
(use-package nim-mode :defer t)
(use-package processing-mode :defer t)
(use-package restart-emacs :defer t)
(use-package diffview :defer t)
(use-package markdown-mode
  :defer t
  :mode (("\\.md$" . markdown-mode)
         ("\\.markdown$" . markdown-mode)))

;; (use-package exec-path-from-shell
;;   :init (exec-path-from-shell-initialize))

(use-package helm
  :config
  (require 'helm-config)
  (setq helm-quick-update nil             ;; blink
        helm-candidate-number-limit 50)
  (helm-push-mark-mode 1)
  (define-key global-map [remap list-buffers] 'helm-buffers-list)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  (add-hook 'helm-grep-mode-hook (lambda () (grep-mode)))
  :bind (("M-x" . helm-M-x)
         ("C-x b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("C-." . helm-imenu-in-all-buffers)
         ("M-y" . helm-show-kill-ring)
         ("C-c h" . helm-command-prefix)
         ("C-c <SPC>" . helm-all-mark-rings)))


(use-package helm-ag
  :defer t
  :config
  (use-package ag)
  (setq helm-ag-base-command "ag --smart-case --nocolor --nogroup")
  (setq helm-ag-insert-at-point 'symbol)
  (add-hook 'helm-ag-mode-hook (lambda () (grep-mode))))

(use-package helm-descbinds
  :defer t
  :config (helm-descbinds-mode))

(use-package helm-swoop
  :defer t
  :config
  (define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
  (define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
  :bind ("M-i" . helm-swoop))

;; TODO: semantic + projectile, projectile

;; (use-package flycheck)
;; (use-package company)
;; (use-package rust-mode
;;   :defer t
;;   :mode (("\\.rs$'" . rust-mode))
;;   :config
;;   (use-package toml-mode :defer t)
;;   (use-package rustfmt :defer t)
;;   (use-package flycheck-rust :defer t)
;;   (use-package company-racer :defer t)
;;   (use-package racer :defer t)
;;   (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
;;   (add-hook 'racer-mode-hook
;;             (lambda ()
;;               (eldoc-mode t)
;;               (company-mode t)))
;;   (add-hook 'rust-mode-hook
;;             (lambda ()
;;               (setq racer-rust-src-path "~/rustc-nightly/src/")
;;               (setq company-tooltip-align-annotations t)
;;               (setq company-minimum-prefix-length 2)
;;               (setq company-idle-delay 0.2)
;;               (racer-mode t)
;;               (flycheck-mode t)
;;               (setq rust-indent-offset 4)))
;;   :bind (("TAB" . company-indent-or-complete-common)
;;          ("M-." . racer-find-definition)))

(use-package projectile
  :diminish projectile-mode
  :defer t
  :config
  (setq projectile-enable-caching t)
  (projectile-global-mode))

(use-package helm-projectile
  :init
  (setq helm-projectile-fuzzy-match nil)
  :config
  (setq projectile-completion-system 'helm)
  (helm-projectile-on)
  (defun my-helm-projectile-buffers-list ()
    (interactive)
    (unless helm-source-buffers-list
      (setq helm-source-buffers-list
            (helm-make-source "Buffers" 'helm-source-buffers)))
    (helm :sources '(helm-source-buffers-list
                     helm-source-projectile-recentf-list
                     helm-source-projectile-files-list
                     helm-source-recentf
                     helm-source-buffer-not-found)
          :buffer "*helm buffers*"
          :keymap helm-buffer-map
          :truncate-lines helm-buffers-truncate-lines))
  (defun my-helm-projectile-ag ()
    (interactive)
    (helm-do-ag (projectile-project-root)))
  (defun my-helm-projectile-curdir-ag ()
    (interactive)
    (helm-do-ag (helm-current-directory)))
  :bind (("M-g" . my-helm-projectile-ag)
         ("M-G" . my-helm-projectile-curdir-ag)
         ("C-c C-f" . helm-projectile-find-file)
         ("C-x b" . my-helm-projectile-buffers-list)))


(use-package smartscan
  :defer t
  :init (add-hook 'prog-mode-hook 'smartscan-mode))   ;; M-n, M-p

(use-package shell
  :defer t
  :config
  (setenv "PAGER" (executable-find "cat"))
  (defun visit-term-buffer ()
    "Create or visit a terminal buffer."
    (interactive)
    (if (not (get-buffer "*shell*"))
        (progn
          (split-window-sensibly (selected-window))
          (other-window 1)
          (shell))
      (switch-to-buffer-other-window "*shell*")))
  :bind ("C-c t" . visit-term-buffer))

(use-package avy
  :config (setq avy-background t)
  :bind ("C-;" . avy-goto-word-or-subword-1)
  :init
  (bind-key "C-;" #'avy-isearch isearch-mode-map))

(use-package ace-jump-helm-line    ;; avy
  :defer t
  :bind ("C-'" . ace-jump-helm-line-execute-action))

(use-package saveplace
  :init (setq-default save-place t))

(use-package vc-darcs
  :config
  (setq vc-disable-async-diff nil)                ;; hotfix
  (add-to-list 'vc-handled-backends 'DARCS t)
  (autoload 'vc-darcs-find-file-hook "vc-darcs")
  (add-hook 'find-file-hooks 'vc-darcs-find-file-hook))

(use-package ninja-mode :defer t)

(use-package clojure-mode
  :defer t
  :if (not (kelly?))
  :pin melpa-stable)

(use-package cider
  :defer t
  :if (not (kelly?))
  :pin melpa-stable
  :config
  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
  ;; (setq cider-auto-mode nil)
  ;; (setq nrepl-log-messages nil)
  ;; (setq nrepl-hide-special-buffers t)
  (setq cider-repl-use-pretty-printing t)
  (setq cider-prompt-save-file-on-load nil)
  (add-hook 'cider-mode-hook 'eldoc-mode)
  :init (add-hook 'clojure-mode-hook 'cider-mode))

(use-package pixie-mode
  :defer t
  :config (add-hook 'pixie-mode-hook #'inf-clojure-minor-mode))

(use-package browse-kill-ring
  :config
  (define-key (current-global-map) [remap yank-pop] 'browse-kill-ring)   ;; remap yank-pop
  (setq browse-kill-ring-replace-yank t))

(use-package whitespace
  :diminish whitespace-mode
  :config
  (setq whitespace-line-column 90)             ;; limit line length
  (setq whitespace-style '(face trailing newline))
  (add-hook 'prog-mode-hook 'whitespace-mode))

(use-package shrink-whitespace
  :bind ("M-\\" . shrink-whitespace))

(use-package expand-region
  :defer t
  :bind ("C-=" . er/expand-region))

(use-package rainbow-mode
  :defer t
  :diminish rainbow-mode
  :init (add-hook 'emacs-lisp-mode-hook 'rainbow-mode))

;; (if (kelly?)
;;   (use-package flatui-theme
;;     :init (load-theme 'flatui t)))

(set-background-color "gray90")

;; vystup z customize-face:
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-background-face ((t (:background "gray90" :foreground "gray60"))))
 '(region ((t (:background "#f1c40f" :distant-foreground "gtk_selection_fg_color")))))

(use-package smart-mode-line
  :config
  (setq sml/no-confirm-load-theme t)
  (sml/setup))

(use-package google-translate
  :commands (google-translate-query-translate)
  :defer t)

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
        smtpmail-starttls-credentials '(("mail.messagingengine.com" 587 nil nil))
        smtpmail-auth-credentials '(("mail.messagingengine.com" 587 "capak@inputwish.com" nil))
        smtpmail-default-smtp-server "mail.messagingengine.com"
        smtpmail-smtp-server "mail.messagingengine.com"
        smtpmail-smtp-service 587))

;; (when (not (kelly?))
;;   (progn
;;     (require 'nnir)
;;     (require 'gnus)
;;     (setq user-mail-address "capak@inputwish.com"
;;           user-full-name  "Libor Čapák"
;;         gnus-select-method
;;         '(nnimap "fastmail"
;;                  (nnimap-address "mail.messagingengine.com")
;;                  (nnimap-server-port 993)
;;                  (nnimap-stream ssl)
;;                  (nnir-search-engine imap)
;;                  ;; press 'E' to expire mail
;;                  (nnmail-expiry-target "nnimap+fastmail:INBOX.Trash")
;;                  (nnmail-expiry-wait 30))
;;         gnus-use-correct-string-widths nil
;;         gnus-permanently-visible-groups ".*\\(Inbox\\|INBOX\\).*"
;;         gnus-thread-sort-functions
;;         '((not gnus-thread-sort-by-date)
;;           (not gnus-thread-sort-by-number))
;;         gnus-use-cache t
;;         gnus-use-adaptive-scoring nil
;;         gnus-save-score nil
;;         gnus-use-scoring nil
;;         gnus-summary-default-score 0
;;         epa-file-cache-passphrase-for-symmetric-encryption t
;;         gnus-read-active-file 'some
;;         gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject)

;;     (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
;;     (add-hook 'gnus-summary-mode-hook 'my-gnus-summary-keys)

;;     (defun my-gnus-summary-keys ()
;;       (local-set-key "y" 'fastmail-archive)
;;       (local-set-key "$" 'fastmail-report-spam))

;;     (defun fastmail-archive ()
;;       (interactive)
;;       (gnus-summary-move-article nil "nnimap+fastmail:INBOX.Archive"))

;;     (defun fastmail-report-spam ()
;;       (interactive)
;;       (gnus-summary-move-article nil "nnimap+fastmail:INBOX.Spam"))))

;; (setq rmail-primary-inbox-list '("imap://capak%40inputwish.com@mail.messagingengine.com"))
;; (setq rmail-movemail-variant-in-use 'mailutils)
;; (setq rmail-remote-password-required t)

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
        mu4e-drafts-folder "/INBOX.Drafts"
        mu4e-sent-folder   "/INBOX.Sent"
        mu4e-trash-folder  "/INBOX.Trash"
        mu4e-maildir-shortcuts
        '( ("/INBOX"         . ?i)
           ("/INBOX.Sent"    . ?s)
           ("/INBOX.Trash"   . ?t)
           ("/INBOX.Archive" . ?a))
        mu4e-bookmarks '(((concat "flag:unread"
                                  " AND NOT flag:trashed"
                                  " AND NOT maildir:/INBOX.Trash"
                                  " AND NOT maildir:/INBOX.Spam")
                          "Unread messages"      ?u)
                         ("date:today..now"                  "Today's messages"     ?t)
                         ("date:7d..now"                     "Last 7 days"          ?w)
                         ("mime:image/*"                     "Messages with images" ?p)
                         ("size:2M..500M"                    "Big messages"         ?b))
        mu4e-get-mail-command "offlineimap -q"
        mu4e-update-interval 600
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
  ;; (set-face-attribute 'mu4e-unread-face nil
  ;;                     :inherit font-lock-preprocessor-face
  ;;                     :bold t)
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
  (defun mu4e-move-to-spam ()
    (interactive)
    (mu4e-mark-set 'move "/INBOX.Spam")
    (mu4e-headers-next))
  (define-key mu4e-headers-mode-map (kbd "c") 'mu4e-move-to-spam)
  (global-set-key (kbd "C-c m") 'mu4e))

;; (use-package mu4e-alert
;;   :if (not (kelly?))
;;   :init
;;   (setq mu4e-alert-interesting-mail-query
;;         (concat "flag:unread"
;;                 " AND NOT flag:trashed"
;;                 " AND NOT maildir:/INBOX.Trash"
;;                 " AND NOT maildir:/INBOX.Spam"))
;;   (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display))

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
    (use-package rtags
      ;; https://github.com/Andersbakken/rtags + https://github.com/rizsotto/Bear
      ;; to create compile_commands.json: bear scons
      :init
      (add-hook 'c-mode-common-hook
                (lambda ()
                  (define-key c-mode-base-map (kbd "M-.") 'rtags-find-symbol-at-point)
                  (define-key c-mode-base-map (kbd "M-,") 'rtags-location-stack-back))))
    (use-package clang-format
      :init
      (setq clang-format-executable
            (if (executable-find "clang-format") "clang-format"
              (if (executable-find "clang-format-3.8") "clang-format-3.8"
                (if (executable-find "clang-format-3.7") "clang-format-3.7"
                  (if (executable-find "clang-format-3.6") "clang-format-3.6"
                    (if (executable-find "clang-format-3.5") "clang-format-3.5"))))))
      (setq clang-format-style (concat "{BasedOnStyle: Google,"
                                       " BinPackParameters: true,"
                                       " IndentWidth: 2,"
                                       " ColumnLimit: 90,"
                                       " AlwaysBreakBeforeMultilineStrings: false,"
                                       " SpacesBeforeTrailingComments: 4,"
                                       " AllowShortFunctionsOnASingleLine: false,"
                                       " NamespaceIndentation: All,"
                                       " BreakBeforeBraces: Linux,"
                                       " UseTab: Never,"
                                       " ConstructorInitializerIndentWidth: 2,"
                                       " ContinuationIndentWidth: 2,"
                                       " Standard: C++11}"))
      :config
      ;; without -sort-includes option:
      (defun clang-format-region (char-start char-end &optional style)
        "Use clang-format to format the code between START and END according to STYLE.
     If called interactively uses the region or the current statement if there
     is no active region.  If no style is given uses `clang-format-style'."
        (interactive
         (if (use-region-p)
             (list (region-beginning) (region-end))
           (list (point) (point))))

        (unless style
          (setq style clang-format-style))

        (let ((start (1- (position-bytes char-start)))
              (end (1- (position-bytes char-end)))
              (cursor (1- (position-bytes (point))))
              (temp-buffer (generate-new-buffer " *clang-format-temp*"))
              (temp-file (make-temp-file "clang-format")))
          (unwind-protect
              (let (status stderr operations)
                (setq status
                      (call-process-region
                       (point-min) (point-max) clang-format-executable
                       nil `(,temp-buffer ,temp-file) nil
                       "-output-replacements-xml"
                       "-assume-filename" (or (buffer-file-name) "")
                       "-style" style
                       "-offset" (number-to-string start)
                       "-length" (number-to-string (- end start))
                       "-cursor" (number-to-string cursor)))
                (setq stderr
                      (with-temp-buffer
                        (insert-file-contents temp-file)
                        (when (> (point-max) (point-min))
                          (insert ": "))
                        (buffer-substring-no-properties
                         (point-min) (line-end-position))))

                (cond
                 ((stringp status)
                  (error "(clang-format killed by signal %s%s)" status stderr))
                 ((not (equal 0 status))
                  (error "(clang-format failed with code %d%s)" status stderr)))

                (with-current-buffer temp-buffer
                  (setq operations (clang-format--extract (car (xml-parse-region)))))

                (let ((replacements (nth 0 operations))
                      (cursor (nth 1 operations))
                      (incomplete-format (nth 2 operations)))
                  (save-excursion
                    (mapc (lambda (rpl)
                            (apply #'clang-format--replace rpl))
                          replacements))
                  (when cursor
                    (goto-char (byte-to-position (1+ cursor))))
                  (message "%s" incomplete-format)
                  (if incomplete-format
                      (message "(clang-format: incomplete (syntax errors)%s)" stderr)
                    (message "(clang-format: success%s)" stderr))))
            (delete-file temp-file)
            (when (buffer-name temp-buffer) (kill-buffer temp-buffer)))))
      (bind-key "C-M-\\" 'clang-format-region c++-mode-map)
      (bind-key "C-i" 'clang-format c++-mode-map))
    (font-lock-add-keywords
     nil
     '((my-c-mode-font-lock-if0 (0 font-lock-comment-face prepend))) 'add-to-end)
    (setq fill-column 90)))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(use-package elpy
  :commands (eply-enable)
  :init (with-eval-after-load 'python (elpy-enable))
  :config
  (remove-hook 'elpy-modules 'elpy-module-yasnippet)
  (when (or (equal system-type 'darwin) (kelly?))
    (remove-hook 'elpy-modules 'elpy-module-flymake)))

(use-package racket-mode
  :if (not (kelly?))
  :defer t
  :config
  (add-hook 'racket-mode-hook          ;; same as C-c C-k
            (lambda ()
              (define-key racket-mode-map (kbd "C-c r") 'racket-run))))

(when (not (kelly?))
  (setq compile-command "scons"))

(diminish 'abbrev-mode)
(diminish 'isearch-mode)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)  ;; symbol-at-point)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-x k")
                '(lambda () (interactive)
                   (let (kill-buffer-query-functions) (kill-buffer))))
(define-key function-key-map "\e[$" (kbd "C-$"))
(define-key function-key-map "\e[%" (kbd "C-%"))
(define-key function-key-map "\e[," (kbd "C-,"))
(define-key function-key-map "\e[;" (kbd "C-;"))
(define-key function-key-map "\e[=" (kbd "C-="))
(define-key function-key-map "\e[." (kbd "C-."))

(define-key global-map (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "M-r") 'recompile)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-c C-g") 'goto-line)

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
