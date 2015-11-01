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
      (string= system-name "idev03")
      (string= system-name "idev03.autokelly.local")))

(setq gc-cons-threshold 20000000)
;; (setq compilation-skip-threshold 2)

(defalias 'after 'with-eval-after-load)
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
  (set-default-font "Menlo 14"))

(when (and window-system (equal system-type 'gnu/linux))
  ;; (set-default-font "Inconsolata 13"))
  (set-default-font "DejaVu Sans Mono 11"))
;; (setq x-alt-keysym 'meta)

(use-package json-mode)
(use-package lua-mode)
(use-package rust-mode)
(use-package swift-mode)
(use-package processing-mode)
(use-package restart-emacs)
(use-package diffview)
(use-package markdown-mode
  :mode (("\\.md$" . markdown-mode)
         ("\\.markdown$" . markdown-mode)))

(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

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
                                   " ColumnLimit: 100,"
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

(use-package helm
  :defer t
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
         ("M-G" . helm-do-grep-ag)
         ("C-c h" . helm-command-prefix)
         ("C-c <SPC>" . helm-all-mark-rings)))

(use-package ag)
(use-package helm-ag
  :config
  (setq helm-ag-insert-at-point 'symbol)
  (add-hook 'helm-ag-mode-hook (lambda () (grep-mode))))

(use-package projectile
  :diminish projectile-mode
  :init (setq projectile-enable-caching t)
  :config (projectile-global-mode))

(use-package helm-descbinds
  :config (helm-descbinds-mode))

(use-package helm-swoop
  :config
  (define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
  (define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
  :bind ("M-i" . helm-swoop))

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
  :bind (("M-g" . helm-projectile-ag)
         ("M-G" . helm-projectile-grep)
         ("C-c C-f" . helm-projectile-find-file)
         ("C-x b" . my-helm-projectile-buffers-list)))

(use-package eshell
  :defer t
  :config
  (setq eshell-command-aliases-list (append '(("l" "ls -lcrt")
                                              ("d" "dired $1")
                                              ("ff" "find-file $1"))
                                            eshell-command-aliases-list))
  (setenv "PAGER" (executable-find "cat"))
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
  :config
  (setq vc-disable-async-diff nil)                ;; hotfix
  (add-to-list 'vc-handled-backends 'DARCS t)
  (autoload 'vc-darcs-find-file-hook "vc-darcs")
  (add-hook 'find-file-hooks 'vc-darcs-find-file-hook))

(use-package ninja-mode)

(use-package clojure-mode
  :pin melpa-stable)

(use-package pixie-mode
  :config (add-hook 'pixie-mode-hook #'inf-clojure-minor-mode))

(use-package browse-kill-ring
  :config
  (define-key (current-global-map) [remap yank-pop] 'browse-kill-ring)   ;; remap yank-pop
  (setq browse-kill-ring-replace-yank t))

(use-package whitespace
  :diminish whitespace-mode
  :config
  (setq whitespace-line-column 100)             ;; limit line length
  (setq whitespace-style '(face trailing newline))
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
 '(region ((t (:background "#f1c40f" :distant-foreground "gtk_selection_fg_color")))))

(use-package smart-mode-line
  :config
  (setq sml/no-confirm-load-theme t)
  (sml/setup))

(use-package nav-flash
  :commands (nav-flash-show)
  :init (setq nav-flash-delay 0.6)
  (add-hook 'imenu-after-jump-hook 'nav-flash-show nil t)
  (defun flash-defun ()
    "flash current defun"
    (interactive)
    (save-restriction
      (narrow-to-defun)
      (nav-flash-show (point-min) (point-max))))
  (defvar nav-flash-show-soon-timer nil)
  (defun nav-flash-show-soon-cancel-timer ()
    (when nav-flash-show-soon-timer
      (cancel-timer nav-flash-show-soon-timer)
      (setq nav-flash-show-soon nil)))
  (defun nav-flash-show-soon (&optional later)
    (nav-flash-show-soon-cancel-timer)
    (setq nav-flash-show-soon-timer
          (run-with-timer (if later 0.4 0.25) nil
                          '(lambda ()
                             (nav-flash-show)))))
  (defun nav-flash-show-later ()
    (nav-flash-show-soon t))
  (add-hook 'focus-in-hook 'nav-flash-show-later)
  (add-hook 'focus-out-hook 'nav-flash-show-soon-cancel-timer)
  (defun recenter-top-bottom-flash ()
    (interactive)
    (call-interactively 'recenter-top-bottom)
    (nav-flash-show))
  (defun move-to-window-line-top-bottom-flash ()
    (interactive)
    (call-interactively 'move-to-window-line-top-bottom)
    (nav-flash-show))
  (defun scroll-up-command-flash ()
    (interactive)
    (call-interactively 'scroll-up-command)
    (nav-flash-show-soon))
  (defun scroll-down-command-flash ()
    (interactive)
    (call-interactively 'scroll-down-command)
    (nav-flash-show-soon))
  :bind (("M-v" . scroll-down-command-flash)
         ;; ("M-r" . move-to-window-line-top-bottom-flash)
         ("C-l" . recenter-top-bottom-flash)
         ("C-v" . scroll-up-command-flash)))

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
        smtpmail-starttls-credentials '(("mail.messagingengine.com" 587 nil nil))
        smtpmail-auth-credentials '(("mail.messagingengine.com" 587 "capak@inputwish.com" nil))
        smtpmail-default-smtp-server "mail.messagingengine.com"
        smtpmail-smtp-server "mail.messagingengine.com"
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
        mu4e-update-interval 300
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

(use-package mu4e-alert
  :if (not (kelly?))
  :init
  (setq mu4e-alert-interesting-mail-query
        (concat "flag:unread"
                " AND NOT flag:trashed"
                " AND NOT maildir:/INBOX.Trash"
                " AND NOT maildir:/INBOX.Spam"))
  (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display))

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

(global-set-key (kbd "C-s") 'isearch-forward-regexp)  ;; symbol-at-point)
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
(global-set-key (kbd "C-c C-g") 'goto-line)

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
