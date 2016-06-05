(setq my-gc-threshold (* 64 1024 1024))

(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold my-gc-threshold)))

(add-hook 'minibuffer-setup-hook
          (lambda () (setq gc-cons-threshold most-positive-fixnum)))

(add-hook 'minibuffer-exit-hook
          (lambda () (setq gc-cons-threshold my-gc-threshold)))

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

(setq package-list '(diminish
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
                     goto-last-change
                     quelpa
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
                     elpy
                     racket-mode
                     hydra
                     go-mode
                     smooth-scrolling
                     unkillable-scratch
                     ;; geiser
                     slime
                     slime-company
                     cff
                     bbdb
                     tide
                     popup
                     quack
                     magit
                     volatile-highlights
                     key-chord
                     ))

(if (file-exists-p "~/.local/share/emacs/site-lisp/rtags")
    (add-to-list 'load-path "~/.local/share/emacs/site-lisp")
  (add-to-list package-list rtags))

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
      auto-save-default nil
      ;; imenu-auto-rescan t
      ;; echo-keystrokes 0.1
      use-dialog-box nil
      uniquify-buffer-name-style 'forward
      ediff-window-setup-function 'ediff-setup-windows-plain
      font-lock-maximum-decoration '((c-mode . 1)
                                     (c++-mode . 1)
                                     (objc-mode . 1)
                                     (t . t))
      vc-diff-switches "-u"
      search-highlight t
      isearch-allow-scroll t
      eval-expression-print-level nil
      user-mail-address "capak@inputwish.com"
      user-full-name  "Libor Čapák"
      google-translate-default-source-language "en"
      google-translate-default-target-language "cs")

(defun kelly? ()
  (or (string= system-name "typhoon.autokelly.local")
      (string= system-name "idev02.autokelly.local")
      (string= system-name "idev03")
      (string= system-name "idev03.autokelly.local")))

(defun dos2unix ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward (string ?\C-m)  nil t)
      (replace-match "" nil t))))

;; (setq compilation-skip-threshold 2)

(setq-default major-mode 'indented-text-mode)    ;; instead fundamental-mode

(delete-selection-mode t)
(setq show-paren-delay 0)   ;; must be set before mode activating
(show-paren-mode 1)
;; (setq show-paren-style 'expression)
(transient-mark-mode t)
(which-function-mode)
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

(require 'diminish)

(require 'volatile-highlights)
(volatile-highlights-mode t)
(vhl/ext/etags/off)
(diminish 'volatile-highlights-mode)

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

(when (equal system-type 'windows-nt)
  (setq ring-bell-function 'ignore)
  (set-frame-font "mononoki-11"))

(when (and window-system (equal system-type 'gnu/linux))
  ;; (set-default-font "Inconsolata 13"))
  (set-frame-font "mononoki-12"))   ;; DejaVu Sans Mono-11.5"))
;; (setq x-alt-keysym 'meta)

(require 'smooth-scrolling)

(require 'unkillable-scratch)
(unkillable-scratch 1)

(require 'goto-last-change)
(global-set-key "\C-x\C-\\" 'goto-last-change)

(when (or (equal system-type 'darwin)
          (equal system-type 'gnu/linux))
  (exec-path-from-shell-initialize)
  (setq python-shell-completion-native-enable nil))

;; Workaround for "ad-handle-definition: `tramp-read-passwd' got redefined".
;; Message is triggered by helm, it is likely missing this require.
(require 'tramp)

(require 'helm-config)
(helm-mode 1)
(diminish 'helm-mode)

(setq helm-candidate-number-limit 50)
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
(super-save-mode 1)
(diminish 'super-save-mode)

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

(require 'key-chord)
(key-chord-define-global "jj" 'avy-goto-word-1)
(key-chord-define-global "jl" 'avy-goto-line)
(key-chord-define-global "jk" 'avy-goto-char)
(key-chord-mode +1)

(define-key helm-map (kbd "C-'") 'ace-jump-helm-line-execute-action)

(if (< emacs-major-version 25)
    (toggle-save-place-globally)
  (save-place-mode 1))

;; (desktop-save-mode 1)

(setq quelpa-update-melpa-p nil)

(quelpa '(vc-darcs :fetcher github :repo "velkyel/vc-darcs"))
(setq vc-disable-async-diff nil)                ;; hotfix
(add-to-list 'vc-handled-backends 'DARCS t)
(autoload 'vc-darcs-find-file-hook "vc-darcs")
(add-hook 'find-file-hooks 'vc-darcs-find-file-hook)

(quelpa '(wren-mode :fetcher github :repo "4d47/wren-mode"))
(require 'wren-mode)

(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

(require 'go-mode-autoloads)
(with-eval-after-load 'go-mode
  (define-key go-mode-map (kbd "M-.")
    (lambda ()
      (interactive)
      (godef-jump (point) current-prefix-arg)))) ;; go get github.com/rogpeppe/godef

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

(with-eval-after-load 'highlight-symbol
  (diminish 'highlight-symbol-mode)
  (setq highlight-symbol-idle-delay 1.0)
  (set-face-background 'highlight-symbol-face "gray82"))

(setq sml/no-confirm-load-theme t)
(sml/setup)
;; (setq sml/theme 'respectful)

(smart-mark-mode)

(global-set-key (kbd "C-a") 'mwim-beginning-of-code-or-line)
;; "C-e" . mwim-end-of-code-or-line

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.mm$" . objc-mode))
(add-to-list 'auto-mode-alist '("\\.\\(glsl\\|vert\\|frag\\|vsh\\|fsh\\|usf\\)\\'" . glsl-mode))
;; ...usf = unreal engine

(setq clang-format-executable
      (if (executable-find "clang-format") "clang-format"
        (if (executable-find "clang-format-3.8") "clang-format-3.8"
          (if (executable-find "clang-format-3.7") "clang-format-3.7"
            (if (executable-find "clang-format37") "clang-format37"
              (if (executable-find "clang-format-3.6") "clang-format-3.6"
                (if (executable-find "clang-format-3.5") "clang-format-3.5")))))))
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

(with-eval-after-load 'company
  (diminish 'company-mode)
  (setq company-idle-delay nil)  ;; 0.1)
  (define-key company-active-map (kbd "\C-n") 'company-select-next)
  (define-key company-active-map (kbd "\C-p") 'company-select-previous)
  (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
  (define-key company-active-map (kbd "M-.") 'company-show-location))

(require 'company)

(require 'rtags)
(require 'popup)

(add-to-list 'company-backends 'company-rtags)
(setq rtags-use-helm t)

(defun my-imenu ()
  (interactive)
  (if (rtags-is-indexed)
      (rtags-imenu)
    (helm-semantic-or-imenu nil)))

(defun my-prog-mode-hook ()
  (highlight-symbol-mode)
  (highlight-symbol-nav-mode)    ;; M-n, M-p
  (company-mode)
  (whitespace-mode)
  (define-key prog-mode-map (kbd "<C-tab>") 'company-complete)
  (define-key prog-mode-map (kbd "C-.") 'my-imenu))

(add-hook 'prog-mode-hook 'my-prog-mode-hook)

(require 'etags)
(defun push-tag-mark () (xref-push-marker-stack))

(when (equal system-type 'gnu/linux)
  (progn
    (require 'pulse)
    (setq pulse-flag nil)))

(defun my-c-mode-common-hook ()
  (setq-local fill-column 90)
  (setq rtags-autostart-diagnostics t
        ))  ;; rtags-show-containing-function t))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(require 'cff)
(add-to-list 'cff-source-regexps '("\\.m$" . (lambda (base) (concat base ".m"))))
(add-to-list 'cff-source-regexps '("\\.mm$" . (lambda (base) (concat base ".mm"))))

(with-eval-after-load 'cc-mode
  (define-key c-mode-base-map (kbd "<C-tab>") 'company-complete)
  (define-key c-mode-base-map (kbd "M-.") 'rtags-find-symbol-at-point)
  (define-key c-mode-base-map (kbd "M-,")
    (lambda ()
      (interactive)
      (if (rtags-is-indexed)
          (rtags-location-stack-back)
        (xref-pop-marker-stack))))    ;; could be jump from scheme or whatever
  (define-key c-mode-base-map (kbd "C-M-\\") 'clang-format-region)
  (define-key c-mode-base-map (kbd "M-?") 'rtags-display-summary)
  (define-key c-mode-base-map (kbd "C-i") 'clang-format)
  (define-key c-mode-base-map (kbd "C-.") 'my-imenu)
  (define-key c-mode-base-map (kbd "M-o") 'cff-find-other-file))

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
    (require 'elpy)
    (setq python-shell-interpreter "python3"
          elpy-rpc-python-command "python3")
    (elpy-enable)
    (remove-hook 'elpy-modules 'elpy-module-yasnippet)
    (when (kelly?)
      (remove-hook 'elpy-modules 'elpy-module-flymake))))

(with-eval-after-load 'racket-mode
  (define-key racket-mode-map (kbd "C-c r") 'racket-run))

(setq lua-default-application "lua5.3")

;; (setq scheme-program-name "chibi-scheme -mchibi.repl -e(repl)")
;; (add-to-list 'auto-mode-alist '("\\.sld\\'" . scheme-mode))
(setq quack-default-program "gsi -:d-")
(require 'quack)

(setq inferior-lisp-program (executable-find "sbcl"))
(slime-setup '(slime-fancy
               slime-asdf
               slime-sbcl-exts
               slime-compiler-notes-tree
               slime-company))
(setq slime-repl-history-remove-duplicates t
      slime-repl-history-trim-whitespaces t
      slime-enable-evaluate-in-emacs t
      slime-auto-start 'always)

(require 'tide)
(with-eval-after-load 'tide
  (define-key tide-mode-map (kbd "C-.") 'imenu))

(add-hook 'typescript-mode-hook
          (lambda ()
            (tide-setup)
            (flycheck-mode 1)
            (setq typescript-indent-level 2
                  flycheck-check-syntax-automatically '(save mode-enabled)
                  company-tooltip-align-annotations t)
            (eldoc-mode 1)
            (company-mode-on)))

(setq compile-command (if (kelly?)
                          "make -k -j 8"
                        "scons"))

(diminish 'abbrev-mode)
(diminish 'isearch-mode)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)  ;; symbol-at-point)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-x k") 'kill-this-buffer)

(define-key function-key-map "\e[$" (kbd "C-$"))
(define-key function-key-map "\e[%" (kbd "C-%"))
(define-key function-key-map "\e[," (kbd "C-,"))
(define-key function-key-map "\e[;" (kbd "C-;"))
(define-key function-key-map "\e[=" (kbd "C-="))
(define-key function-key-map "\e[." (kbd "C-."))

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "M-r") (lambda ()
                              (interactive)
                              (call-interactively (if (get-buffer "*compilation*")
                                                      'recompile
                                                    'compile))))
;; (global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-c C-g") 'goto-line)

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
      gnus-summary-line-format "%U%R%z %(%&user-date;  %-22,22f  %B%s%)\n"
      gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
      gnus-thread-sort-functions
      '((not gnus-thread-sort-by-date)
        (not gnus-thread-sort-by-number))
      gnus-message-archive-group "nnimap+fastmail:INBOX.Sent"
      gnus-gcc-mark-as-read t
      gnus-use-cache t
      gnus-sum-thread-tree-false-root ""
      gnus-sum-thread-tree-indent " "
      gnus-sum-thread-tree-leaf-with-other "├► "
      gnus-sum-thread-tree-root ""
      gnus-sum-thread-tree-single-leaf "╰► "
      gnus-sum-thread-tree-vertical "│"
      gnus-interactive-exit nil
      message-kill-buffer-on-exit t
      gnus-large-newsgroup nil
      epa-file-cache-passphrase-for-symmetric-encryption t
      gnus-read-active-file 'some
      gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject
      mm-discouraged-alternatives '("text/html" "text/richtext")
      ;; gnus-parameters
      ;; '((".*"
      ;;    (display . all)))
      )

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

(require 'bbdb)

(setq bbdb-file "~/.bbdb"
      bbdb-complete-mail-allow-cycling t)

(bbdb-initialize 'message 'gnus)

(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)

(add-hook 'message-mode-hook
          '(lambda ()
             (flyspell-mode t)
             (local-set-key (kbd "<tab>") 'bbdb-complete-mail)))

(set-background-color "gray85")
(set-face-attribute 'default
                    nil
                    :background "gray85")   ;; terminal

(set-face-attribute 'helm-source-header
                    nil
                    :height 0.9)

(set-face-attribute 'font-lock-comment-face
                    nil
                    ;; :height 0.9
                    :slant 'italic)

(set-face-attribute 'region
                    nil
                    :background "#f1c40f"
                    :distant-foreground "gtk_selection_fg_color")

(set-face-attribute 'mode-line
                    nil
                    :background "gray95"
                    :box '(:line-width -1 :style released-button))

(set-face-attribute 'mode-line-inactive
                    nil
                    :background "gray80"
                    :box '(:line-width -1 :style released-button))

(set-face-attribute 'rtags-skippedline
                    nil
                    :background "gray80")

(set-face-attribute 'rtags-warnline
                    nil
                    :background "#ccccff")

(set-face-attribute 'rtags-errline
                    nil
                    :background "#eeb0b0")

(require 'highlight-indentation)
(set-face-attribute 'highlight-indentation-face     ;; elpy
                    nil
                    :background "gray90")
