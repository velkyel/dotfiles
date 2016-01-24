(setq gc-cons-threshold 20000000)

(require 'package)

(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(defun display-startup-echo-area-message nil nil)
(menu-bar-mode -1)

(if window-system
    (progn
      (tool-bar-mode -1)
      (tooltip-mode -1)
      (scroll-bar-mode -1)
      (fringe-mode 2)))

(setq inhibit-startup-message t
      initial-scratch-message nil)

(defconst package-list '(diminish
                         exec-path-from-shell
                         json-mode
                         lua-mode
                         haskell-mode
                         nim-mode
                         processing-mode
                         restart-emacs
                         diffview
                         ag
                         helm
                         helm-ag
                         helm-descbinds
                         helm-swoop
                         projectile
                         helm-projectile
                         super-save
                         anzu
                         shell
                         avy
                         ace-jump-helm-line
                         saveplace
                         quelpa
                         quelpa-use-package
                         ninja-mode
                         clojure-mode
                         cider
                         pixie-mode
                         browse-kill-ring
                         easy-kill
                         whitespace
                         shrink-whitespace
                         expand-region
                         visual-regexp
                         eldoc
                         rainbow-mode
                         smart-mode-line
                         smart-mark
                         google-translate
                         mwim
                         glsl-mode
                         clang-format
                         highlight-symbol
                         company
                         popup
                         rtags
                         elpy
                         racket-mode
                         hydra
                         go-mode
                         smooth-scrolling
                         ))

(setq package-pinned-packages
      '((cider . "melpa-stable")
        (clojure-mode . "melpa-stable")))

(unless package-archive-contents
  (package-refresh-contents))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(setq compilation-ask-about-save nil
      compilation-always-kill t
      compilation-scroll-output 'first-error
      next-line-add-newlines nil
      load-prefer-newer t
      require-final-newline t
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
      eval-expression-print-level nil
      user-mail-address "capak@inputwish.com"
      user-full-name  "Libor Čapák")

(defun kelly? ()
  (or (string= system-name "typhoon.autokelly.local")
      (string= system-name "idev02.autokelly.local")
      (string= system-name "idev03")
      (string= system-name "idev03.autokelly.local")))

;; (setq compilation-skip-threshold 2)

(setq-default major-mode 'indented-text-mode)    ;; instead fundamental-mode

(delete-selection-mode t)
(setq show-paren-delay 0)   ;; must be set before mode activating
(show-paren-mode 1)
;; (setq show-paren-style 'expression)
(transient-mark-mode t)
(which-function-mode)
;; (semantic-mode 1)
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
  (set-frame-font "Menlo 15"))

(when (and window-system (equal system-type 'gnu/linux))
  ;; (set-default-font "Inconsolata 13"))
  (set-frame-font "DejaVu Sans Mono 11"))
;; (setq x-alt-keysym 'meta)

(require 'diminish)
(require 'smooth-scrolling)

(when (equal system-type 'darwin)
  (exec-path-from-shell-initialize))

;; Workaround for "ad-handle-definition: `tramp-read-passwd' got redefined".
;; Message is triggered by helm, it is likely missing this require.
(require 'tramp)

(require 'helm-config)
(helm-mode 1)
(diminish 'helm-mode)

(setq helm-quick-update nil             ;; blink
      helm-candidate-number-limit 50)
(helm-push-mark-mode 1)
(advice-add 'helm-ff-filter-candidate-one-by-one     ;; skip ".." pattern (C-l)
            :around (lambda (fcn file)
                      (unless (string-match "\\(?:/\\|\\`\\)\\.\\{2\\}\\'" file)
                        (funcall fcn file))))
(add-hook 'helm-grep-mode-hook (lambda () (grep-mode)))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)

(global-set-key [remap list-buffers] 'helm-buffers-list)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-h a") 'helm-apropos)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-set-key (kbd "C-c <SPC>") 'helm-all-mark-rings)

(setq helm-ag-base-command "ag --smart-case --nocolor --nogroup")
(setq helm-ag-insert-at-point 'symbol)
(add-hook 'helm-ag-mode-hook (lambda () (grep-mode)))

(helm-descbinds-mode)

(require 'helm-swoop)
(define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
(global-set-key (kbd "M-i") 'helm-swoop)

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

(setq projectile-enable-caching t)
(projectile-global-mode)
(diminish 'projectile-mode)

(setq helm-projectile-fuzzy-match nil)
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

(global-set-key (kbd "M-g") (lambda ()
                              (interactive)
                              (helm-do-ag (projectile-project-root))))

(global-set-key (kbd "M-G") (lambda ()
                              (interactive)
                              (helm-do-ag (helm-current-directory))))

(global-set-key (kbd "C-c C-f") 'helm-projectile-find-file)
(global-set-key (kbd "C-x b") 'my-helm-projectile-buffers-list)

(require 'super-save)
(super-save-initialize)

(global-anzu-mode 1)
(diminish 'anzu-mode)

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
(global-set-key (kbd "C-c t") 'visit-term-buffer)

(setq avy-background t)
(global-set-key (kbd "C-;") 'avy-goto-word-or-subword-1)
(define-key isearch-mode-map (kbd "C-;") 'avy-isearch)

(define-key helm-map (kbd "C-'") 'ace-jump-helm-line-execute-action)

(toggle-save-place-globally)

(require 'quelpa-use-package)
(setq quelpa-update-melpa-p nil)
(quelpa-use-package-activate-advice)   ;; quelpa-upgrade
(use-package vc-darcs
  :quelpa (vc-darcs :fetcher github :repo "velkyel/vc-darcs")
  :config
  (setq vc-disable-async-diff nil)                ;; hotfix
  (add-to-list 'vc-handled-backends 'DARCS t)
  (autoload 'vc-darcs-find-file-hook "vc-darcs")
  (add-hook 'find-file-hooks 'vc-darcs-find-file-hook))

(require 'go-mode-autoloads)

(add-hook 'pixie-mode-hook #'inf-clojure-minor-mode)

(global-set-key [remap yank-pop] 'browse-kill-ring)   ;; remap yank-pop
(setq browse-kill-ring-replace-yank t)

(global-set-key [remap kill-ring-save] 'easy-kill)
(global-set-key [remap mark-sexp] 'easy-mark)

(with-eval-after-load 'whitespace
  (diminish 'whitespace-mode))
(setq whitespace-line-column 90
      whitespace-style '(face trailing newline))

(global-set-key (kbd "M-\\") 'shrink-whitespace)
(global-set-key (kbd "C-=") 'er/expand-region)

(global-set-key (kbd "C-c r") 'vr/replace)

(diminish 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
(add-hook 'ielm-mode-hook 'eldoc-mode)

(with-eval-after-load 'rainbow-mode
  (diminish 'rainbow-mode))
(add-hook 'emacs-lisp-mode-hook 'rainbow-mode)

(set-background-color "gray90")

(with-eval-after-load 'highlight-symbol
  (diminish 'highlight-symbol-mode)
  (setq highlight-symbol-idle-delay 1.0)
  (set-face-background 'highlight-symbol-face "gray82"))

;; vystup z customize-face:
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-background-face ((t (:background "gray90" :foreground "gray50"))))
 '(region ((t (:background "#f1c40f" :distant-foreground "gtk_selection_fg_color"))))
 '(rtags-skippedline ((t (:background "gray90" :foreground "gray50"))))
 '(wl-highlight-summary-important-flag-face ((t (:foreground "red"))) t))

(setq sml/no-confirm-load-theme t)
(sml/setup)
;; (setq sml/theme 'respectful)

(smart-mark-mode)

(global-set-key (kbd "C-a") 'mwim-beginning-of-code-or-line)
;; "C-e" . mwim-end-of-code-or-line

(add-to-list 'auto-mode-alist '("\\.mm$" . objc-mode))
(add-to-list 'auto-mode-alist '("\\.\\(glsl\\|vert\\|frag\\|vsh\\|fsh\\|usf\\)\\'" . glsl-mode))
;; ...usf = unreal engine

(setq clang-format-executable
      (if (executable-find "clang-format") "clang-format"
        (if (executable-find "clang-format-3.8") "clang-format-3.8"
          (if (executable-find "clang-format-3.7") "clang-format-3.7"
            (if (executable-find "clang-format-3.6") "clang-format-3.6"
              (if (executable-find "clang-format-3.5") "clang-format-3.5"))))))
(setq clang-format-style (concat "{BasedOnStyle: Google,"
                                 " BreakBeforeBraces: Mozilla,"
                                 " BinPackParameters: true,"
                                 " BreakBeforeBinaryOperators: NonAssignment,"
                                 " IndentWidth: 2,"
                                 " ColumnLimit: 90,"
                                 " AlwaysBreakBeforeMultilineStrings: false,"
                                 " SpacesBeforeTrailingComments: 4,"
                                 " AccessModifierOffset: -2,"
                                 " AllowShortFunctionsOnASingleLine: Inline,"
                                 " NamespaceIndentation: All,"
                                 " UseTab: Never,"
                                 " ConstructorInitializerIndentWidth: 2,"
                                 " ContinuationIndentWidth: 2,"
                                 " PointerAlignment: Left,"
                                 " DerivePointerAlignment: false,"
                                 " Standard: Cpp11}"))

(with-eval-after-load 'company (diminish 'company-mode))
(setq company-idle-delay 0.1)

(require 'rtags)
(require 'popup)
(require 'company)
(require 'company-rtags)

(defun my-imenu ()
  (interactive)
  (if (rtags-is-indexed)
      (rtags-imenu)
    (helm-semantic-or-imenu nil)))

;; https://github.com/Andersbakken/rtags + https://github.com/rizsotto/Bear
;; to create compile_commands.json: bear scons

(defun my-prog-mode-hook ()
  (highlight-symbol-mode)
  (highlight-symbol-nav-mode)    ;; M-n, M-p
  (company-mode)
  (whitespace-mode)
  (define-key prog-mode-map (kbd "<C-tab>") 'company-complete)
  (define-key prog-mode-map (kbd "C-.") 'my-imenu))

(add-hook 'prog-mode-hook 'my-prog-mode-hook)

(defun my-c-mode-common-hook ()
  (setq fill-column 90)
  (setq company-backends '(company-rtags company-files))
  (setq rtags-completions-enabled t
        rtags-display-current-error-as-tooltip t
        rtags-autostart-diagnostics t
        rtags-use-helm t
        rtags-show-containing-function t)
        ;;rtags-track-container t)
  ;; (add-hook 'find-file-hook
  ;;           (lambda ()
  ;;             (setq mode-line-misc-info '(("" rtags-cached-current-container "")))))
  ;;                   ;; (and (rtags-is-indexed)
  ;;                   ;;      '(:eval rtags-cached-current-container)))))
  ;;             ;; (message header-line-format)))
  (rtags-diagnostics)
  (define-key c-mode-base-map (kbd "<C-tab>") 'company-complete)
  (define-key c-mode-base-map (kbd "M-.") 'rtags-find-symbol-at-point)
  (define-key c-mode-base-map (kbd "M-,") 'rtags-location-stack-back)
  (define-key c-mode-base-map (kbd "C-M-\\") 'clang-format-region)
  (define-key c-mode-base-map (kbd "M-?") 'rtags-display-summary)
  (define-key c-mode-base-map (kbd "C-i") 'clang-format)
  (define-key c-mode-base-map (kbd "C-.") 'my-imenu))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
;; (setq cider-auto-mode nil)
;; (setq nrepl-log-messages nil)
;; (setq nrepl-hide-special-buffers t)
(setq cider-repl-use-pretty-printing t)
(setq cider-prompt-save-file-on-load nil)
(add-hook 'cider-mode-hook 'eldoc-mode)
(add-hook 'clojure-mode-hook 'cider-mode)

(with-eval-after-load 'python
  (progn
    (elpy-enable)
    (remove-hook 'elpy-modules 'elpy-module-yasnippet)
    (when (or (equal system-type 'darwin) (kelly?))
      (remove-hook 'elpy-modules 'elpy-module-flymake))))

(with-eval-after-load 'racket-mode
  (define-key racket-mode-map (kbd "C-c r") 'racket-run))

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

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "M-r") 'recompile)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-c C-g") 'goto-line)

;; (require 'company)
;; (setq company-backends (delete 'company-semantic company-backends)
;;       company-global-modes '(not term-mode)
;;       company-transformers '(company-sort-by-occurrence)
;;       company-minimum-prefix-length 2
;;       company-selection-wrap-around t
;;       company-show-numbers t
;;       company-require-match nil
;;       company-dabbrev-downcase nil)
;; ;; (setq company-idle-delay 0.1)
;; (add-hook 'prog-mode-hook (lambda () (company-mode 1)))

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-auth-credentials "~/.authinfo"
      ;;smtpmail-stream-type 'ssl
      starttls-use-gnutls t
      smtpmail-starttls-credentials '(("mail.messagingengine.com" 587 nil nil))
      smtpmail-auth-credentials '(("mail.messagingengine.com" 587 "capak@inputwish.com" nil))
      smtpmail-default-smtp-server "mail.messagingengine.com"
      smtpmail-smtp-server "mail.messagingengine.com"
      smtpmail-smtp-service 587)

(require 'nnir)
(require 'gnus)

(global-set-key (kbd "C-c m") 'gnus)

(setq gnus-select-method '(nnimap "fastmail"
                                  (nnimap-address "mail.messagingengine.com")
                                  (nnimap-server-port 993)
                                  (nnimap-stream ssl)
                                  (nnir-search-engine imap)
                                  ;; press E to expire mail
                                  (nnmail-expiry-target "nnimap+fastmail:INBOX.Trash")
                                  (nnmail-expiry-wait 7))
      gnus-permanently-visible-groups ".*\\(Inbox\\|INBOX\\).*"
      gnus-thread-sort-functions
      '((not gnus-thread-sort-by-date)
        (not gnus-thread-sort-by-number))
      gnus-message-archive-group "nnimap+fastmail:INBOX.Sent"
      gnus-use-cache t
      ;; gnus-use-adaptive-scoring nil
      ;; gnus-save-score nil
      ;; gnus-use-scoring nil
      ;; gnus-summary-default-score 0
      gnus-interactive-exit nil
      message-kill-buffer-on-exit t
      epa-file-cache-passphrase-for-symmetric-encryption t
      gnus-read-active-file 'some
      gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject
      mm-discouraged-alternatives '("text/html" "text/richtext"))

;; (defun fastmail-archive ()
;;   (interactive)
;;   (gnus-summary-move-article nil "nnimap+fastmail:INBOX.Archive"))

(defun fastmail-report-spam ()
  (interactive)
  (guns-summary-move-article nil "nnimap+fastmail:INBOX.Spam"))

(defun my-gnus-summary-keys ()
  ;; (local-set-key "y" 'fastmail-archive)
  (local-set-key "$" 'fastmail-report-spam))

;; (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
;; (add-hook 'gnus-summary-mode-hook 'my-gnus-summary-keys)

;; (require 'bbdb)
;; (setq 'bbdb-complete-mail-allow-cycling t)
;; (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
;; (add-hook 'message-mode-hook
;;           '(lambda ()
;;              (bbdb-initialize 'message)
;;              (bbdb-initialize 'gnus)
;;              (local-set-key "<TAB>" 'bbdb-complete-name)))

(eval-after-load 'gnus-group
  '(progn
     (defhydra hydra-gnus-group (:color blue)
       "Do?"
       ("a" gnus-group-list-active "REMOTE groups A A")
       ("l" gnus-group-list-all-groups "LOCAL groups L")
       ("c" gnus-topic-catchup-articles "Read all c")
       ("G" gnus-group-make-nnir-group "Search server G G")
       ("g" gnus-group-get-new-news "Refresh g")
       ("s" gnus-group-enter-server-mode "Servers")
       ("m" gnus-group-new-mail "Compose m OR C-x m")
       ("#" gnus-topic-mark-topic "mark #")
       ("q" nil "cancel"))
     ;; y is not used by default
     (define-key gnus-group-mode-map "y" 'hydra-gnus-group/body)))

;; gnus-summary-mode
(eval-after-load 'gnus-sum
  '(progn
     (defhydra hydra-gnus-summary (:color blue)
       "Do?"
       ("n" gnus-summary-insert-new-articles "Refresh / N")
       ("f" gnus-summary-mail-forward "Forward C-c C-f")
       ("!" gnus-summary-tick-article-forward "Mail -> disk !")
       ("p" gnus-summary-put-mark-as-read "Mail <- disk")
       ("c" gnus-summary-catchup-and-exit "Read all c")
       ("e" gnus-summary-resend-message-edit "Resend S D e")
       ("R" gnus-summary-reply-with-original "Reply with original R")
       ("r" gnus-summary-reply "Reply r")
       ("W" gnus-summary-wide-reply-with-original "Reply all with original S W")
       ("w" gnus-summary-wide-reply "Reply all S w")
       ("#" gnus-topic-mark-topic "mark #")
       ("q" nil "cancel"))
     ;; y is not used by default
     (define-key gnus-summary-mode-map "y" 'hydra-gnus-summary/body)))

;; gnus-article-mode
(eval-after-load 'gnus-art
  '(progn
     (defhydra hydra-gnus-article (:color blue)
       "Do?"
       ("f" gnus-summary-mail-forward "Forward")
       ("R" gnus-article-reply-with-original "Reply with original R")
       ("r" gnus-article-reply "Reply r")
       ("W" gnus-article-wide-reply-with-original "Reply all with original S W")
       ("o" gnus-mime-save-part "Save attachment at point o")
       ("w" gnus-article-wide-reply "Reply all S w")
       ("q" nil "cancel"))
     ;; y is not used by default
     (define-key gnus-article-mode-map "y" 'hydra-gnus-article/body)))

(eval-after-load 'message
  '(progn
     (defhydra hydra-message (:color blue)
       "Do?"
       ("ca" mml-attach-file "Attach C-c C-a")
       ("cc" message-send-and-exit "Send C-c C-c")
       ("q" nil "cancel"))
     (global-set-key (kbd "C-c C-y") 'hydra-message/body)))
