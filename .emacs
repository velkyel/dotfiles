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
      ;; echo-keystrokes 0.1
      use-dialog-box nil
      uniquify-buffer-name-style 'forward
      ediff-window-setup-function 'ediff-setup-windows-plain
      font-lock-maximum-decoration '((racket-mode . t) (t . 1))
      vc-diff-switches "-u"
      search-highlight t
      isearch-allow-scroll t
      user-mail-address "capak@inputwish.com"
      user-full-name  "Libor Čapák")

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

(when (equal system-type 'darwin)
  (use-package exec-path-from-shell
    :init (exec-path-from-shell-initialize)))

(use-package helm
  :diminish helm-mode
  :config
  (require 'helm-config)
  (setq helm-quick-update nil             ;; blink
        helm-candidate-number-limit 50)
  (helm-push-mark-mode 1)
  (define-key global-map [remap list-buffers] 'helm-buffers-list)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  (add-hook 'helm-grep-mode-hook (lambda () (grep-mode)))
  :init (helm-mode 1)
  :bind (("M-x" . helm-M-x)
         ("C-x b" . helm-buffers-list)
         ("C-h a" . helm-apropos)
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

(use-package super-save
  :config (super-save-initialize))

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

(use-package quelpa
  :init
  (setq quelpa-update-melpa-p nil))

(use-package quelpa-use-package    ;; quelpa-upgrade
  :config
  (quelpa-use-package-activate-advice))

(use-package vc-darcs
  :quelpa (vc-darcs :fetcher github :repo "velkyel/vc-darcs")
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

(use-package easy-kill
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark)))

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

(use-package visual-regexp
  :bind ("C-c r" . vr/replace))

(use-package eldoc
  :diminish eldoc-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
  (add-hook 'ielm-mode-hook 'eldoc-mode))

(use-package rainbow-mode
  :defer t
  :diminish rainbow-mode
  :init (add-hook 'emacs-lisp-mode-hook 'rainbow-mode))

;; ;; (load-theme 'tango t)
(set-background-color "gray90")

;; vystup z customize-face:
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-background-face ((t (:background "gray90" :foreground "gray50"))))
 '(region ((t (:background "#f1c40f" :distant-foreground "gtk_selection_fg_color"))))
 '(wl-highlight-summary-important-flag-face ((t (:foreground "red"))) t))

(use-package smart-mode-line
  :config
  (setq sml/no-confirm-load-theme t)
  (sml/setup))

(use-package smart-mark
  :config (smart-mark-mode))

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

(use-package clang-format
  :defer t
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
  (add-hook 'c-mode-common-hook
            (lambda ()
              (define-key c-mode-base-map (kbd "C-M-\\") 'clang-format-region)
              (define-key c-mode-base-map (kbd "C-i") 'clang-format))))

(use-package rtags
  ;; https://github.com/Andersbakken/rtags + https://github.com/rizsotto/Bear
  ;; to create compile_commands.json: bear scons
  :defer t
  :init
  (add-hook 'c-mode-common-hook
            (lambda ()
              (define-key c-mode-base-map (kbd "M-.") 'rtags-find-symbol-at-point)
              (define-key c-mode-base-map (kbd "M-,") 'rtags-location-stack-back))))

(defun my-c-mode-common-hook ()
  (progn
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

(use-package wanderlust
  :init
  (autoload 'wl "wl" "wanderlust" t)
  (autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
  (autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

  ;; mail-user-agent
  (autoload 'wl-user-agent-compose "wl-draft" nil t)
  (if (boundp 'mail-user-agent)
      (setq mail-user-agent 'wl-user-agent))
  (if (fboundp 'define-mail-user-agent)
      (define-mail-user-agent
        'wl-user-agent
        'wl-user-agent-compose
        'wl-draft-send
        'wl-draft-kill
        'mail-send-hook))

  ;; fastmail imap
  (setq elmo-imap4-default-server "mail.messagingengine.com"
        elmo-imap4-default-user "capak@inputwish.com"
        elmo-imap4-default-authenticate-type 'clear
        elmo-imap4-default-port '993
        elmo-imap4-default-stream-type 'ssl)

  ;; smtp
  (setq wl-smtp-connection-type 'starttls
        wl-smtp-posting-port 587
        wl-smtp-authenticate-type "plain"
        wl-smtp-posting-user "capak@inputwish.com"
        wl-smtp-posting-server "mail.messagingengine.com"
        wl-local-domain "inputwish.com"
        wl-message-id-domain "mail.messagingengine.com")

  (add-hook 'wl-summary-sync-updated-hook
            (lambda () (wl-summary-rescan "date" 1)))    ;; reverse

  (setq wl-demo nil
        wl-folder-check-async t
        wl-summary-showto-folder-regexp ".*sent.*"

        wl-message-ignored-field-list '("^.*:")    ;; ignore all fields
        wl-message-visible-field-list
        '("^\\(To\\|Cc\\):"
          "^\\(From\\|Reply-To\\):"
          "^Subject:"
          "^Organization:"
          "^\\(Posted\\|Date\\):")

        ;; '/' -- filter folder, napr: /since:yesterday/%INBOX
        wl-thread-indent-level 3
        wl-thread-have-younger-brother-str "+"
        wl-thread-youngest-child-str "+"
        wl-thread-vertical-str "|"
        wl-thread-horizontal-str "-"
        wl-thread-space-str " "

        wl-summary-width nil
        wl-summary-indent-length-limit nil
        wl-summary-always-sticky-folder-list t

        ;; wl-folder-hierarchy-access-folders
        ;; '("^.\\([^/.]+[/.]\\)*[^/.]+\\(:\\|@\\|$\\)"
        ;;   "^-[^.]*\\(:\\|@\\|$\\)"
        ;;   "^@$"
        ;;   "^'$")

        wl-from "Libor Čapák <capak@inputwish.com>"
        wl-default-folder "%INBOX"
        wl-draft-folder "%INBOX.Drafts"
        wl-trash-folder "%INBOX.Trash"
        wl-fcc "%INBOX.Sent"
        wl-fcc-force-as-read t
        wl-default-spec "%"

        wl-show-plug-status-on-modeline t

        ;; TODO: combine with wl-biff-notify-hook?
        global-mode-string (cons '(wl-modeline-biff-status
                                   wl-modeline-biff-state-on
                                   wl-modeline-biff-state-off)
                                 global-mode-string)

        wl-interactive-exit nil
        wl-interactive-send nil))

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

(global-set-key (kbd "C-c m") 'wl)

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
