(setq my-gc-threshold (* 64 1024 1024))

(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold my-gc-threshold)))

(add-hook 'minibuffer-setup-hook
          (lambda () (setq gc-cons-threshold most-positive-fixnum)))

(add-hook 'minibuffer-exit-hook
          (lambda () (setq gc-cons-threshold my-gc-threshold)))

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(menu-bar-mode -1)

(defun setup-my-fringe ()
  (fringe-mode '(8 . 0)))

(when (display-graphic-p)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (scroll-bar-mode -1)
  (add-hook 'window-setup-hook 'setup-my-fringe)
  (add-hook 'after-make-frame-functions 'setup-my-fringe))

(defconst *osx* (eq system-type 'darwin))
(defconst *linux* (eq system-type 'gnu/linux))
(defconst *windows* (eq system-type 'windows-nt))

(defconst *kelly* (or (string= system-name "typhoon.autokelly.local")
                      (string= system-name "idev02")
                      (string= system-name "idev02.autokelly.local")
                      (string= system-name "idev03")
                      (string= system-name "idev03.autokelly.local")
                      (string= system-name "idev05")
                      (string= system-name "idev05.autokelly.local")))

(setq inhibit-startup-message t
      initial-scratch-message nil)

(setq package-list '(packed
                     auto-package-update
                     bind-key
                     auto-compile
                     diminish
                     exec-path-from-shell
                     json-mode
                     flymake-lua
                     haskell-mode
                     restart-emacs
                     helm
                     projectile
                     helm-projectile
                     super-save
                     anzu
                     avy
                     goto-chg
                     unfill
                     quelpa
                     ninja-mode
                     clojure-mode
                     inf-clojure
                     shrink-whitespace
                     expand-region
                     visual-regexp
                     rainbow-mode
                     smart-mark
                     google-translate
                     glsl-mode
                     clang-format
                     highlight-symbol
                     company
                     elpy
                     unkillable-scratch
                     slime
                     slime-company
                     cff
                     popup
                     magit
                     volatile-highlights
                     key-seq
                     dumb-jump
                     shackle
                     helm-xref
                     crux
                     web-mode
                     js2-mode
                     rust-mode
                     smart-hungry-delete
                     helpful
                     dired-collapse
                     dired-rainbow
                     ;; mu4e-alert
                     ;; lsp-mode
                     ;; cquery
                     parinfer
                     elm-mode))

(setq *rtags* (file-exists-p "~/rtags/src"))
(when *rtags* (add-to-list 'load-path "~/rtags/src"))

(set-language-environment "czech")
(setq default-input-method "czech-qwerty")
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(unless package-archive-contents
  (package-refresh-contents))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'packed)
(require 'auto-compile)
(auto-compile-on-load-mode 1)
(auto-compile-on-save-mode 1)

(require 'subr-x)    ;; string-trim
(require 'diminish)

(setq compilation-ask-about-save nil
      compilation-always-kill t
      compilation-scroll-output 'first-error
      next-line-add-newlines nil
      ring-bell-function 'ignore
      load-prefer-newer t
      require-final-newline t
      column-number-mode t
      make-backup-files nil
      delete-auto-save-files t
      auto-save-default nil
      large-file-warning-threshold nil
      ;; imenu-auto-rescan t
      ;; echo-keystrokes 0.1
      use-dialog-box nil
      shift-select-mode nil
      uniquify-buffer-name-style 'forward
      ediff-window-setup-function 'ediff-setup-windows-plain
      vc-diff-switches "-u"
      search-highlight t
      isearch-allow-scroll t
      eval-expression-print-level nil
      mail-user-agent 'gnus-user-agent
      user-mail-address "capak@inputwish.com"
      user-full-name  "Libor Čapák"
      ;; scroll-conservatively 101
      ;;scroll-preserve-screen-position 'always
      google-translate-default-source-language "en"
      google-translate-default-target-language "cs")

(setq-default indent-tabs-mode nil
              line-spacing nil
              tab-width 4
              py-indent-offset 4)

(defun dos2unix ()
  (interactive)
  (set-buffer-file-coding-system 'unix))

;; (setq compilation-skip-threshold 2)

;; (setq-default major-mode 'text-mode)    ;; instead fundamental-mode

(delete-selection-mode t)
(setq show-paren-delay 0)   ;; must be set before mode activating
(show-paren-mode 1)
;; (setq show-paren-style 'expression)
;; (electric-pair-mode 1)
(transient-mark-mode t)
(which-function-mode)
(winner-mode t)   ;; C-c <left|right>
;; (type-break-mode 1)
;; (type-break-query-mode 1)
;; (type-break-mode-line-message-mode 1)
(defalias 'yes-or-no-p 'y-or-n-p)

(require 'crux)

(setenv "PAGER" (executable-find "cat"))
(bind-key "C-c t" '(lambda ()
                     (interactive)
                     (crux-start-or-switch-to 'eshell "*eshell*")))

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

(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

;; (require 'recentf)
;; (add-to-list 'recentf-exclude "bookmarks")

(when *osx*
  ;; (setq mac-command-modifier 'meta)
  ;; (setq mac-option-modifier nil)
  (setq mac-option-modifier 'nil)
  (setq mac-command-modifier 'meta)
  (setq ns-function-modifier 'hyper)
  (set-frame-font "mononoki-16"))

(when *windows*
  (set-frame-font "mononoki-10.4"))

(when (and (display-graphic-p) *linux*)
  (set-frame-font "hack 11"))

(require 'unkillable-scratch)
(unkillable-scratch 1)

(require 'goto-chg)
(bind-key "C-x C-\\" 'goto-last-change)

(when (or *osx* *linux*)
  (exec-path-from-shell-initialize))

(setq python-shell-completion-native-enable nil)

(require 'projectile)
(setq projectile-enable-caching t)
(projectile-global-mode)
(diminish 'projectile-mode)
(add-to-list 'projectile-globally-ignored-files ".DS_Store")
(add-to-list 'projectile-globally-ignored-directories ".build")
(add-to-list 'projectile-globally-ignored-directories "build")
(add-to-list 'projectile-globally-ignored-directories ".cquery_cached_index")

(require 'helm)
(require 'helm-config)
(require 'helm-grep)
(helm-mode 1)   ;; completion-read etc..
(diminish 'helm-mode)
(setq helm-candidate-number-limit 100)
(setq helm-buffer-max-length 32)
(setq helm-display-header-line nil)
(setq helm-mode-fuzzy-match t)
(setq helm-completion-in-region-fuzzy-match t)

;; (helm-push-mark-mode 1)
(bind-keys :map helm-map
           ("<tab>" . helm-execute-persistent-action)
           ("C-i" . helm-execute-persistent-action)
           ("C-z" . helm-select-action))

(add-hook 'helm-grep-mode-hook 'grep-mode)
(setq helm-grep-save-buffer-name-no-confirm 1)
(setq helm-grep-file-path-style 'relative)

(setq projectile-completion-system 'helm)
(helm-projectile-on)

(bind-key "M-g" '(lambda ()
                   (interactive)
                   (if *windows* (helm-grep-do-git-grep "")
                     (helm-grep-ag (projectile-project-root) nil))))

;; ;; helm-ag
;; (setq helm-ag-insert-at-point t)
;; (setq helm-ag--action-save-buffer #'helm-grep-save-results
;; (bind-key "M-g" 'helm-projectile-ag)

;; TODO:
;; - thing-at-point
;; - spustit helm-grep z isearch
;; - helm-ag (volano z helm-projectile-ag) ma v gr pro "args flags 0" neprazdny vysledek, zatimco helm-grep-ag nic nenajde

(bind-key* "M-G"     ;; overrides any mode-specific bindings
           (if *windows* 'helm-grep-do-git-grep
             '(lambda ()
                (interactive)
                (helm-grep-ag (helm-current-directory) nil))))    ;; nebo expand-file-name default-directory ?

(bind-key "M-i" 'helm-occur-from-isearch isearch-mode-map)

;; (require 'helm-for-files)    ;; helm-source-recentf
;;(setq helm-M-x t)

(defun my-helm-projectile-buffers-list ()
  (interactive)
  (unless helm-source-buffers-list
    (setq helm-source-buffers-list
          (helm-make-source "Buffers" 'helm-source-buffers)))
  (helm :sources '(helm-source-buffers-list
                   helm-source-projectile-files-list
                   helm-source-buffer-not-found)
        :buffer "*helm buffers*"
        :keymap helm-buffer-map
        :truncate-lines helm-buffers-truncate-lines))

(bind-keys ([remap list-buffers] . my-helm-projectile-buffers-list)
           ("M-x" . helm-M-x)
           ("C-x b" . my-helm-projectile-buffers-list)
           ("C-h a" . helm-apropos)
           ("C-x C-f" . helm-find-files)
           ("M-y" . helm-show-kill-ring)
           ("M-i" . helm-occur)
           ("C-c h" . helm-command-prefix)
           ("C-c <SPC>" . helm-all-mark-rings)
           ("C-c C-r" . helm-resume))

(require 'helpful)
(bind-keys ("C-h f" . helpful-function)
           ("C-h v" . helpful-variable)
           ("C-h F" . helpful-command)
           ("C-h M" . helpful-macro))

(require 'smart-hungry-delete)
(bind-keys ("<backspace>" . smart-hungry-delete-backward-char)
           ("C-d" . smart-hungry-delete-forward-char))

(require 'shackle)
(setq shackle-rules
      '(("*Help*" :align t :select t)
        ("\\`\\*cider-repl .*" :regexp t :align t :size 0.2)
        ((inferior-scheme-mode "*shell*" "*eshell*") :popup t))
      ;; shackle-default-rule '(:select t)
      shackle-default-size 0.4
      shackle-inhibit-window-quit-on-same-windows t)
(shackle-mode)

(require 'json-mode)    ;; C-c C-p show-path; C-c C-f beautify
(add-hook 'json-mode-hook (lambda ()
                            (make-local-variable 'js-indent-level)
                            (setq js-indent-level 2)))

(defun nxml-where ()
  "Display the hierarchy of XML elements the point is on as a path."
  (interactive)
  (let ((path nil))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (< (point-min) (point)) ;; Doesn't error if point is at beginning of buffer
                    (condition-case nil
                        (progn
                          (nxml-backward-up-element) ; always returns nil
                          t)
                      (error nil)))
          (setq path (cons (xmltok-start-tag-local-name) path)))
        (if (called-interactively-p t)
            (message "/%s" (mapconcat 'identity path "/"))
          (format "/%s" (mapconcat 'identity path "/")))))))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(when *kelly*
  (add-to-list 'auto-mode-alist '("\\.tem?\\'" . web-mode)))

(require 'super-save)
(super-save-mode 1)
(diminish 'super-save-mode)

(require 'anzu)
(global-anzu-mode 1)
(diminish 'anzu-mode)

(require 'avy)
(setq avy-background t)
(bind-keys ("C-;" . avy-goto-word-or-subword-1)
           ("M-m" . avy-goto-char-timer))
(bind-key "C-;" 'avy-isearch isearch-mode-map)

(require 'key-seq)
(key-seq-define-global "jj" 'avy-goto-word-or-subword-1)
(key-seq-define-global "jl" 'goto-line)
(key-seq-define-global "jk" 'avy-goto-char-timer)
;; (key-seq-define-global "JJ" 'crux-switch-to-previous-buffer)
;; (key-seq-define-global "bb" 'my-helm-projectile-buffers-list)
(key-chord-mode +1)

;; (require 'jump-char)
;; (bind-key "M-m" 'jump-char-forward)
;; (bind-key "M-M" 'jump-char-backward)

(require 'unfill)
(bind-key [remap fill-paragraph] 'unfill-toggle)

(require 'dired)
(require 'dired-collapse)

(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 4))

(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(bind-key (vector 'remap 'beginning-of-buffer) 'dired-back-to-top dired-mode-map)
(bind-key (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom dired-mode-map)

(put 'dired-find-alternate-file 'disabled nil)
(bind-key "RET" 'dired-find-alternate-file dired-mode-map)

(add-hook 'dired-mode-hook #'dired-collapse-mode)

(require 'dired-rainbow)

(defconst my-dired-media-files-extensions
  '("mp3" "mp4" "MP3" "MP4" "avi" "mpg" "flv" "ogg")
  "Media files.")

(defconst my-dired-image-files-extensions
  '("png" "jpg" "jpeg" "tga" "bmp" "dds")
  "Image files.")

(dired-rainbow-define html "#4e9a06" ("htm" "html" "xhtml"))
(dired-rainbow-define media "#ce5c00" my-dired-media-files-extensions)
(dired-rainbow-define image "#5c00ce" my-dired-image-files-extensions)

;; boring regexp due to lack of imagination
(dired-rainbow-define log (:inherit default :italic t) ".*\\.log")

;; highlight executable files, but not directories
(dired-rainbow-define-chmod executable-unix "#228b22" "-[rw-]+x.*")

(save-place-mode 1)
;; (desktop-save-mode 1)

(setq quelpa-update-melpa-p nil)

(quelpa '(vc-darcs :fetcher github :repo "velkyel/vc-darcs"))
(setq vc-disable-async-diff nil)                ;; hotfix
(add-to-list 'vc-handled-backends 'DARCS t)
(require 'vc-darcs)
(add-hook 'find-file-hooks 'vc-darcs-find-file-hook)

(require 'dumb-jump)
(setq dumb-jump-selector 'helm
      dumb-jump-prefer-searcher 'ag)    ;; because https://github.com/jacktasia/dumb-jump/issues/129

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq js-indent-level 2)
(bind-keys :map js2-mode-map
           ("M-." . dumb-jump-go)
           ("M-," . dumb-jump-back)
           ("C-." . imenu))

(quelpa '(inf-js :fetcher github :repo "velkyel/inf-js"))
(require 'inf-js)
(setq inf-js-program '("localhost" . 5555))
(add-hook 'js2-mode-hook 'inf-js-minor-mode)
;; (js2-imenu-extras-mode 1)

(quelpa '(hlsl-mode :fetcher github :repo "darfink/hlsl-mode"))
(require 'hlsl-mode)
(add-to-list 'auto-mode-alist '("\\.hlsl\\'" . hlsl-mode))

;; (quelpa '(inf-femtolisp :fetcher github :repo "velkyel/inf-femtolisp"))
;; (autoload 'inf-femtolisp "inf-femtolisp" "Run an inferior Femtolisp process" t)
;; (autoload 'inf-femtolisp-minor-mode "inf-femtolisp")
;; (setq inf-femtolisp-program '("localhost" . 5555))
;; (add-hook 'scheme-mode-hook 'inf-femtolisp-minor-mode)

(when (file-exists-p "~/ctrifle/misc")
  (add-to-list 'load-path "~/ctrifle/misc")
  (require 'trifle-mode))

(bind-key "C-x g" 'magit-status)
(bind-key "C-x M-g" 'magit-dispatch-popup)

;; (with-eval-after-load 'magit
;;   (setq magit-completing-read-function 'ivy-completing-read))

(bind-key "C-w" '(lambda ()
                   (interactive)
                   (if (use-region-p)
                       (call-interactively 'kill-region)
                     (crux-kill-whole-line))))

(bind-key "C-a" 'crux-move-beginning-of-line)
(bind-key "C-c d" 'crux-duplicate-current-line-or-region)

(require 'whitespace)
(diminish 'whitespace-mode)
(setq whitespace-line-column 90
      whitespace-style '(face trailing newline))

(require 'shrink-whitespace)
(bind-key "M-\\" 'shrink-whitespace)

(require 'expand-region)
(bind-key "C-=" 'er/expand-region)

(require 'visual-regexp)
(bind-key "C-c r" 'vr/replace)

(diminish 'eldoc-mode)
(setq eldoc-idle-delay 0.2)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
(add-hook 'ielm-mode-hook 'eldoc-mode)

(require 'rainbow-mode)
(diminish 'rainbow-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-mode)
(add-hook 'js2-mode-hook 'rainbow-mode)

(require 'highlight-symbol)
(diminish 'highlight-symbol-mode)
(setq highlight-symbol-idle-delay 0.5)
(set-face-background 'highlight-symbol-face "gray78")
;; (set-face-attribute 'highlight-symbol-face nil :underline t :background "gray85")

(require 'smart-mark)
(smart-mark-mode)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.mm$" . objc-mode))
(add-to-list 'auto-mode-alist '("\\.\\(glsl\\|vert\\|frag\\|vsh\\|fsh\\|usf\\|sc\\)\\'" . glsl-mode))
;; ...usf = unreal engine, sc = bgfx

(require 'company)
(diminish 'company-mode)
(setq company-idle-delay nil)  ;; 0.1)
(when *windows*
  (delete 'company-clang company-backends))
(bind-keys :map company-active-map
           ("C-n" . company-select-next)
           ("C-p" . company-select-previous)
           ("C-d" . company-show-doc-buffer)
           ("M-." . company-show-location))

(require 'flymake)
(defvar flymake-mode-map (make-sparse-keymap))
(bind-keys :map flymake-mode-map
           ("C-M-n" . flymake-goto-next-error)
           ("C-M-p" . flymake-goto-prev-error))

(or (assoc 'flymake-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
          (cons (cons 'flymake-mode flymake-mode-map)
                minor-mode-map-alist)))

(quelpa '(lua-mode :fetcher github :repo "velkyel/lua-mode"))
(require 'lua-mode)
(require 'flymake-lua)
(setq flymake-luac-program "luac5.3")
(add-hook 'lua-mode-hook 'flymake-lua-load)
(setq lua-default-application '("localhost" . 5555))
(bind-keys :map lua-mode-map
           ("C-M-x" . lua-send-proc)
           ("M-." . dumb-jump-go)
           ("M-," . 'dumb-jump-back))

(when *rtags*
  (require 'rtags)
  (setq rtags-display-result-backend 'helm)
  (setq rtags-imenu-syntax-highlighting t))

(defun my-imenu ()
  (interactive)
  (if (and *rtags* (rtags-is-indexed))
      (rtags-imenu)
    (helm-imenu-in-all-buffers)))

(defun my-non-special-modes-setup ()
  (setq indicate-empty-lines t)
  (whitespace-mode)
  (goto-address-mode))

(defun my-prog-modes-hook ()
  (my-non-special-modes-setup)
  (make-local-variable 'comment-auto-fill-only-comments)
  (setq comment-auto-fill-only-comments t)
  (highlight-symbol-mode)
  (highlight-symbol-nav-mode)    ;; M-n, M-p
  (goto-address-prog-mode)
  (company-mode)
  ;; (semantic-mode 1)
  ;; (delete '(scheme-mode . semantic-default-scheme-setup) semantic-new-buffer-setup-functions)
  (bind-keys :map prog-mode-map
             ("<C-tab>" . company-complete)
             ("C-." . my-imenu)))

(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'text-mode-hook 'my-non-special-modes-setup)
(add-hook 'diff-mode-hook 'my-non-special-modes-setup)

(add-hook 'prog-mode-hook 'my-prog-modes-hook)

(require 'helm-xref)
(setq xref-show-xrefs-function 'helm-xref-show-xrefs)

(require 'parinfer)
(add-hook 'clojure-mode-hook #'parinfer-mode)
(add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
(add-hook 'scheme-mode-hook #'parinfer-mode)
(add-hook 'lisp-mode-hook #'parinfer-mode)

(require 'scheme)

(defun run-s7 ()
  (interactive)
  (require 'cmuscheme)
  (setq scheme-program-name "s7")
  (if (not (comint-check-proc "*scheme*"))
      (let ((cmdlist (list '("localhost" . 5555))))
        (set-buffer (apply 'make-comint "scheme" (car cmdlist) nil nil))
        (inferior-scheme-mode)))
  (setq scheme-buffer "*scheme*")
  (pop-to-buffer-same-window "*scheme*"))

(setq pulse-delay .06)
;; (when *linux*
;;   (progn
;;     (require 'pulse)
;;     (setq pulse-flag nil)))

(defun fontify-string (str mode)
  "Return STR fontified according to MODE."
  (with-temp-buffer
    (insert str)
    (delay-mode-hooks (funcall mode))
    (font-lock-default-function mode)
    (font-lock-default-fontify-region
     (point-min) (point-max) nil)
    (buffer-string)))

(require 'compile)
(bind-key "C-c C-t"
          '(lambda ()
             (interactive)
             (compilation-set-skip-threshold (mod (1+ compilation-skip-threshold) 3)))
          compilation-mode-map)
(setq compile-command "ninja")

(defun my-c-mode-common-hook ()
  (when *rtags*
    (add-to-list 'company-backends 'company-rtags))
  (setq-local fill-column 90))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(require 'cff)
(add-to-list 'cff-source-regexps '("\\.m$" . (lambda (base) (concat base ".m"))))
(add-to-list 'cff-source-regexps '("\\.mm$" . (lambda (base) (concat base ".mm"))))

;; (require 'cquery)
;; ;; https://github.com/cquery-project/cquery/wiki/Getting-started
;; (setq cquery-executable (expand-file-name "~/cquery/build/release/bin/cquery"))
;; (setq cquery-extra-init-params '(:index (:comments 2) :cacheFormat "msgpack"))
;; (setq cquery-sem-highlight-method nil)   ;; 'font-lock)

;; (require 'lsp-mode)
;; (setq lsp-highlight-symbol-at-point nil
;;       lsp-enable-flycheck nil
;;       lsp-enable-indentation nil
;;       lsp-enable-eldoc nil)

;; (add-hook 'c-mode-hook #'lsp-cquery-enable)
;; (add-hook 'c++-mode-hook #'lsp-cquery-enable)

;; (require 'lsp-imenu)
;; (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)

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
                                 " Standard: Cpp11,"
                                 " SortIncludes: false}"))

(setq clang-format-executable "clang-format")   ;; ev symlink in ~/bin

(require 'clang-format)
(fset 'c-indent-region 'clang-format-region)

(defun my-goto-symbol ()
  (interactive)
  (save-buffer)
  (deactivate-mark)
  (xref-push-marker-stack)
  (or (and *rtags*
           (rtags-is-indexed)
           (rtags-find-symbol-at-point))
      (dumb-jump-go)))

(bind-keys :map c-mode-base-map
           ("<C-tab>" . company-complete)
           ("C-M-\\" . clang-format-region)
           ("C-i" . clang-format)
           ("C-." . my-imenu)
           ("M-." . my-goto-symbol)
           ("M-," . xref-pop-marker-stack)
           ("M-o" . cff-find-other-file))

(when *rtags*
  (bind-key "M-?" 'rtags-display-summary c-mode-base-map))

(setq inf-clojure-program '("localhost" . 9999))   ;; "planck"
(add-hook 'clojure-mode-hook 'inf-clojure-minor-mode)
(add-hook 'clojure-mode-hook 'eldoc-mode)
;; (add-hook 'clojure-mode-hook 'smartparens-mode)
(add-hook 'inf-clojure-mode-hook 'eldoc-mode)

(add-hook 'python-mode-hook
          '(lambda ()
             (setq-local eldoc-mode nil)))

(with-eval-after-load 'python
  (if *windows*
      (progn
        (setq python-shell-interpreter "python.exe"))
    (progn
      (require 'elpy)
      (setq python-shell-interpreter "python3")
      (setq elpy-rpc-python-command "python3")
      (setq elpy-eldoc-show-current-function nil)
      (elpy-enable)
      (remove-hook 'elpy-modules 'elpy-module-yasnippet)
      (when *kelly*
        (remove-hook 'elpy-modules 'elpy-module-flymake)))))

;; (require 'geiser)
(setq geiser-active-implementations '(racket))

(require 'slime-autoloads)
(setq slime-lisp-implementations '((sbcl ("sbcl" "--noinform") :coding-system utf-8-unix))
      slime-default-lisp 'sbcl
      slime-repl-history-remove-duplicates t
      slime-repl-history-trim-whitespaces t
      slime-enable-evaluate-in-emacs t
      slime-auto-start 'always
      slime-contribs '(slime-fancy
                       slime-asdf
                       slime-sbcl-exts
                       slime-compiler-notes-tree
                       slime-company
                       slime-repl))

(diminish 'abbrev-mode)
(diminish 'isearch-mode)

(defun my-goto-match-beginning ()
  (when (and (not isearch-mode-end-hook-quit) isearch-forward isearch-other-end)
    (goto-char isearch-other-end)))

(defadvice isearch-exit (after my-goto-match-beginning activate)
  "Go to beginning of match."
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end)))

(add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)

(bind-keys ("C-s" . isearch-forward-regexp) ;; symbol-at-point)
           ("C-r" . isearch-backward-regexp)
           ("C-M-s" . isearch-forward)
           ("C-M-r" . isearch-backward)
           ("C-x k" . kill-this-buffer))

;; suspend-frame:
(unbind-key "C-z")
(unbind-key "C-x C-z")

(define-key function-key-map "\e[$" (kbd "C-$"))
(define-key function-key-map "\e[%" (kbd "C-%"))
(define-key function-key-map "\e[," (kbd "C-,"))
(define-key function-key-map "\e[;" (kbd "C-;"))
(define-key function-key-map "\e[=" (kbd "C-="))
(define-key function-key-map "\e[." (kbd "C-."))

(bind-key "RET" 'newline-and-indent)
(bind-key "S-RET" 'crux-smart-open-line)
(bind-key "M-r" '(lambda ()
                   (interactive)
                   (call-interactively (if (get-buffer "*compilation*")
                                           'recompile
                                         'compile))))
;; (bind-key "M-o" 'other-window)

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-auth-credentials "~/.authinfo"
      ;;smtpmail-stream-type 'ssl
      starttls-use-gnutls t
      smtpmail-starttls-credentials '(("mail.messagingengine.com" 587 nil nil))
      smtpmail-auth-credentials '(("mail.messagingengine.com" 587 "capak@inputwish.com" nil))
      smtpmail-default-smtp-server "mail.messagingengine.com"
      smtpmail-smtp-server "mail.messagingengine.com"
      smtpmail-smtp-service 587)

(autoload 'gnus "gnus" "Read network news." t)
(global-set-key (kbd "C-c m") 'gnus)

(setq gnus-select-method '(nnimap "fastmail"
                                  (nnimap-address "mail.messagingengine.com")
                                  (nnimap-server-port 993)
                                  (nnimap-stream ssl)
                                  (nnir-search-engine imap)
                                  ;; press E to expire mail
                                  (nnmail-expiry-target "nnimap+fastmail:INBOX.Trash"))
      gnus-permanently-visible-groups ".*\\(Inbox\\|INBOX\\).*"
      gnus-summary-line-format "%U%R%z %(%&user-date;  %-22,22f  %B%s%)\n"
      gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
      gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date)  ;; gnus-thread-sort-by-date))
      gnus-message-archive-group "nnimap+fastmail:INBOX.Sent"
      gnus-gcc-mark-as-read t
      gnus-use-cache t
      gnus-cacheable-groups "^nnimap"
      gnus-sum-thread-tree-false-root ""
      gnus-sum-thread-tree-indent " "
      gnus-sum-thread-tree-leaf-with-other "├► "
      gnus-sum-thread-tree-root ""
      gnus-sum-thread-tree-single-leaf "╰► "
      gnus-sum-thread-tree-vertical "│"
      gnus-interactive-exit nil
      message-kill-buffer-on-exit t
      gnus-large-newsgroup nil
      gnus-read-active-file 'some
      gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject
      mm-discouraged-alternatives '("text/html" "text/richtext")
      gnus-inhibit-startup-message t
      gnus-agent-expire-days 4
      gnus-use-scoring nil
      gnus-parameters
      '((".*"
         (display . all))))

(setq nnmail-expiry-wait-function
      (lambda (group)
        (cond ((string= group "INBOX") 'immediate)
              (t 'never))))

;; (defun fastmail-archive ()
;;   (interactive)
;;   (gnus-summary-move-article nil "nnimap+fastmail:INBOX.Archive"))

;; (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
;; (add-hook 'gnus-summary-mode-hook 'my-gnus-summary-keys)

;; (when (not *windows*)
;;   (add-to-list 'load-path (if *osx* "/usr/local/share/emacs/site-lisp/mu/mu4e"
;;                             "/usr/share/emacs/site-lisp/mu4e"))
;;   (require 'mu4e)
;;   (require 'mu4e-contrib)
;;   (setq mail-user-agent 'mu4e-user-agent
;;         mu4e-maildir (expand-file-name "~/Maildir")
;;         mu4e-drafts-folder "/INBOX.Drafts"
;;         mu4e-sent-folder "/INBOX.Sent"
;;         mu4e-trash-folder "/INBOX.Trash"
;;         ;;mu4e-refile-folder . "/INBOX.Archive"
;;         ;; mu4e-sent-message-behavior 'delete
;;         mu4e-maildir-shortcuts
;;         '(("/INBOX" . ?i)
;;           ("/INBOX.Sent" . ?s)
;;           ("/INBOX.Trash" . ?t)
;;           ("/INBOX.Archive" . ?a))
;;         mu4e-get-mail-command "offlineimap -o"
;;         mu4e-update-interval 300
;;         mu4e-headers-skip-duplicates t
;;         mu4e-confirm-quit nil
;;         mu4e-date-format-long "%d.%m.%Y"
;;         mu4e-headers-date-format "%d.%m.%y"
;;         mu4e-view-show-images t
;;         mu4e-hide-index-messages t
;;         mu4e-view-show-addresses t
;;         mu4e-completing-read-function 'completing-read
;;         mu4e-headers-leave-behavior 'apply
;;         mu4e-html2text-command (if *osx*
;;                                    "textutil -stdin -format html -convert txt -stdout"
;;                                  "html2text -utf8 -width 72")
;;         message-kill-buffer-on-exit t)
;;   (add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)
;;   (add-hook 'message-mode-hook
;;             '(lambda ()
;;                (my-non-special-modes-setup)
;;                (flyspell-mode t)))
;;   (bind-key "C-c m" 'mu4e)

;;   (require 'mu4e-alert)
;;   (setq mu4e-alert-interesting-mail-query "flag:unread maildir:/INBOX")
;;   (mu4e-alert-enable-mode-line-display)

;;   (require 'smtpmail)
;;   (setq smtpmail-auth-credentials (expand-file-name "~/.authinfo")
;;         message-send-mail-function 'smtpmail-send-it
;;         starttls-use-gnutls t
;;         smtpmail-starttls-credentials '(("mail.messagingengine.com" 587 nil nil))
;;         smtpmail-default-smtp-server "mail.messagingengine.com"
;;         smtpmail-smtp-server "mail.messagingengine.com"
;;         smtpmail-smtp-service 587))

(require 'org)

(defun what-face (pos) ;; under cursor
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(set-background-color "gray85")
(set-face-attribute 'default
                    nil
                    :background "gray85")   ;; terminal

(set-face-attribute 'js2-external-variable
                    nil
                    :foreground "red")

(set-face-attribute 'avy-background-face
                    nil
                    :foreground "gray50")

(set-face-attribute 'helm-ff-executable
                    nil
                    :foreground "#228b22")

(set-face-attribute 'helm-selection
                    nil
                    :background "#a5e8be")

(set-face-attribute 'helm-visible-mark
                    nil
                    :background "#f1c40f")

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

(with-eval-after-load 'popup
  (set-face-attribute 'popup-face
                      nil
                      :background "#dddd30"))

(with-eval-after-load 'highlight-indentation
  (set-face-attribute 'highlight-indentation-face     ;; elpy
                      nil
                      :background "gray90"))

(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file :noerror)
