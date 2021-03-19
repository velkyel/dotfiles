;; -*- lexical-binding: t -*-

(setq my-gc-threshold (* 64 1024 1024))

(setq max-lisp-eval-depth 50000)
(setq max-specpdl-size 5000)

(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold my-gc-threshold)))

(add-hook 'minibuffer-setup-hook
          (lambda () (setq gc-cons-threshold most-positive-fixnum)))

(add-hook 'minibuffer-exit-hook
          (lambda () (setq gc-cons-threshold my-gc-threshold)))

(menu-bar-mode -1)
(blink-cursor-mode 1)

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

(defconst *kelly* (let ((name (system-name)))
                    (or (string= name "idev02")
                        (string= name "idev02.autokelly.local")
                        (string= name "idev03")
                        (string= name "idev03.autokelly.local")
                        (string= name "idev06")
                        (string= name "idev06.autokelly.local"))))

(setq inhibit-startup-message t
      frame-inhibit-implied-resize t
      initial-scratch-message nil)

(setq package-list '(packed
                     bind-key
                     auto-package-update    ; script upgrade-emacs-packages.sh
                     auto-compile
                     exec-path-from-shell
                     json-mode
                     haskell-mode
                     restart-emacs
                     imenu-anywhere
                     projectile
                     helm
                     helm-xref
                     helm-projectile
                     ;; helm-dired-history
                     helm-org-rifle
                     super-save
                     avy
                     goto-chg
                     unfill
                     quelpa
                     ninja-mode
                     clojure-mode
                     lua-mode
                     shrink-whitespace
                     expand-region
                     visual-regexp
                     visual-regexp-steroids
                     rainbow-mode
                     smart-mark
                     google-translate
                     glsl-mode
                     clang-format
                     highlight-symbol
                     popup
                     magit
                     volatile-highlights
                     key-seq
                     dumb-jump
                     shackle
                     crux
                     web-mode
                     js2-mode
                     rust-mode
                     dired-subtree
                     dired-rainbow
                     dired-recent
                     dired-narrow
                     markdown-mode
                     org-bullets
                     smartparens
                     vc-darcs
                     flycheck
                     persistent-scratch
                     unkillable-scratch
                     comment-or-uncomment-sexp
                     shader-mode
                     goto-last-point
                     janet-mode
                     minions
                     hydra
                     w3m
                     semi    ;; for w3m
                     restclient
                     elpy
                     ))

(set-language-environment "czech")
(setq default-input-method "czech-qwerty")
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-refresh-contents)
(package-initialize)

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'quelpa)

(require 'auto-package-update)
(setq auto-package-update-delete-old-versions t)
(auto-package-update-maybe)
(add-hook 'auto-package-update-before-hook
          (lambda () (message "I will update packages now") (quelpa-upgrade-all)))

(require 'packed)
(require 'auto-compile)
(auto-compile-on-load-mode 1)
(auto-compile-on-save-mode 1)

(require 'subr-x)    ;; string-trim

(require 'minions)
(minions-mode 1)

(require 'uniquify)

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
      create-lockfiles nil
      large-file-warning-threshold nil
      ;; imenu-auto-rescan t
      ;; echo-keystrokes 0.1
      use-dialog-box nil
      shift-select-mode nil
      uniquify-buffer-name-style 'forward
      ediff-window-setup-function 'ediff-setup-windows-plain
      search-highlight t
      isearch-allow-scroll t
      eval-expression-print-level nil
      mail-user-agent 'gnus-user-agent
      user-mail-address "capak@inputwish.com"
      user-full-name  "Libor Capak"
      ;;scroll-preserve-screen-position 'always
      scroll-margin 4
      scroll-conservatively 101
      ;; c-hungry-delete-key t
      vc-follow-symlinks t
      calendar-week-start-day 1
      font-lock-maximum-decoration '((c++-mode . 1)
                                     (t . t))
      enable-local-eval t)

(setq-default indent-tabs-mode nil
              line-spacing nil
              tab-width 4
              py-indent-offset 4)

(global-unset-key (kbd "S-<down-mouse-1>"))
(global-unset-key (kbd "<mouse-3>"))
(global-unset-key (kbd "S-<mouse-3>"))

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
(windmove-default-keybindings)
(savehist-mode 1)

(persistent-scratch-setup-default)
(require 'unkillable-scratch)
(setq unkillable-scratch-behavior 'do-nothing
      unkillable-scratch-do-not-reset-scratch-buffer t)
(unkillable-scratch t)

(require 'crux)

(setenv "PAGER" (executable-find "cat"))

(bind-key "C-c t" #'(lambda ()
                      (interactive)
                      (crux-start-or-switch-to 'shell "*shell*")))

(require 'volatile-highlights)
(volatile-highlights-mode t)
(vhl/ext/etags/off)

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

;; (setq global-auto-revert-non-file-buffers t)
;; (global-auto-revert-mode 1)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 30)
;; (add-to-list 'recentf-exclude "bookmarks")

(when *osx*
  ;; (setq mac-command-modifier 'meta)
  ;; (setq mac-option-modifier nil)
  (setq mac-option-modifier 'nil)
  (setq mac-command-modifier 'meta)
  (setq ns-function-modifier 'hyper)
  (add-to-list 'default-frame-alist '(font . "hack 13")))

(when *windows*
  (add-to-list 'default-frame-alist '(font . "hack 11")))

(when (and (display-graphic-p) *linux*)
  (add-to-list 'default-frame-alist '(font . "hack 11")))

(require 'goto-chg)
(bind-key "C-x C-\\" 'goto-last-change)

(require 'goto-last-point)
(goto-last-point-mode)
(bind-key "C-<" 'goto-last-point)

(when (or *osx* *linux*)
  (require 'exec-path-from-shell)
  (setq exec-path-from-shell-arguments (remove "-i" exec-path-from-shell-arguments))   ;; optimization
  (exec-path-from-shell-initialize))

(when *windows*
  (setq eww-download-directory "c:\\Users\\el\\Downloads"))

(setq python-shell-completion-native-enable nil)

(require 'helm)
(require 'helm-config)
(require 'helm-grep)
(require 'helm-files)
(require 'helm-for-files)

(helm-mode +1)

(setq helm-display-header-line nil
      helm-mode-fuzzy-match t
      helm-buffer-max-length 32)

(bind-keys :map helm-map
           ("<tab>" . helm-execute-persistent-action)
           ("TAB" . helm-execute-persistent-action)
           ("C-z" . helm-select-action))

(add-hook 'helm-grep-mode-hook 'grep-mode)
(setq helm-grep-save-buffer-name-no-confirm 1
      helm-grep-file-path-style 'relative)
;; (setq helm-follow-mode-persistent t)

(require 'projectile)
(require 'helm-projectile)
(setq projectile-indexing-method 'native
      projectile-enable-caching t
      projectile-completion-system 'helm)
(projectile-global-mode)
(bind-key "C-x C-p" 'projectile-find-file)

(defun my-helm-switch-buffer ()
  (interactive)
  (unless helm-source-buffers-list
    (setq helm-source-buffers-list
          (helm-make-source "Buffers" 'helm-source-buffers)))
  (helm :sources (if (projectile-project-p)
                     '(helm-source-buffers-list
                       helm-source-recentf
                       helm-source-projectile-files-list
                       helm-source-buffer-not-found)
                   '(helm-source-buffers-list
                     helm-source-recentf
                     helm-source-buffer-not-found))
        :buffer "*helm buffers*"
        :keymap helm-buffer-map
        :truncate-lines helm-buffers-truncate-lines))

(bind-keys ("M-x" . helm-M-x)
           ("C-x C-b" . my-helm-switch-buffer)
           ("C-x b" . my-helm-switch-buffer)
           ("C-h a" . helm-apropos)
           ("C-x C-f" . helm-find-files)
           ("M-y" . helm-show-kill-ring)
           ("M-i" . helm-occur)
           ("C-c h" . helm-command-prefix)
           ("C-c <SPC>" . helm-all-mark-rings)
           ("C-c C-r" . helm-resume))

(require 'grep)
(add-to-list 'grep-find-ignored-files ".DS_Store")
(add-to-list 'grep-find-ignored-files "TAGS")
(add-to-list 'grep-find-ignored-directories ".build")
(add-to-list 'grep-find-ignored-directories "build")
(add-to-list 'grep-find-ignored-directories "_darcs")

(bind-key "M-g" #'(lambda ()
                    (interactive)
                    (save-some-buffers t nil)
                    (helm-grep-ag (projectile-project-root) current-prefix-arg)))

(bind-key* "M-G" #'(lambda ()
                     (interactive)
                     (save-some-buffers t nil)
                     (helm-grep-ag (helm-current-directory) current-prefix-arg)))

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
(require 'hideshow)
(add-hook 'json-mode-hook (lambda ()
                            (make-local-variable 'js-indent-level)
                            (setq js-indent-level 2)
                            (hs-minor-mode 1)))
(bind-key (kbd "<C-return>") 'hs-toggle-hiding json-mode-map)

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

(require 'avy)
(setq avy-background t)
(bind-keys ("C-;" . avy-goto-word-or-subword-1)
           ("M-m" . avy-goto-char-timer))
(bind-key "C-;" 'avy-isearch isearch-mode-map)

(require 'key-seq)
(key-seq-define-global "jj" 'avy-goto-word-or-subword-1)
(key-seq-define-global "jl" 'goto-line)
(key-seq-define-global "jk" 'avy-goto-char-timer)
(key-seq-define-global "JJ" 'crux-switch-to-previous-buffer)

(key-chord-mode +1)

(require 'unfill)
(bind-key [remap fill-paragraph] 'unfill-toggle)

(require 'dired)
(require 'dired-subtree)
(bind-key "<tab>" 'dired-subtree-toggle dired-mode-map)

(when *osx* (setq dired-use-ls-dired nil))

(setq dired-recursive-copies 'always
      dired-recursive-deletes 'always
      dired-isearch-filenames 'dwim)

(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(eval-after-load "dired-aux"
   '(add-to-list 'dired-compress-file-suffixes '("\\.zip\\'" ".zip" "unzip")))

;; (require 'peep-dired)
;; (bind-key "P" 'peep-dired dired-mode-map)
;; (setq peep-dired-cleanup-on-disable t
;;       peep-dired-ignore-extensions '("mkv" "webm" "mp4" "mp3" "ogg" "iso"))

(require 'wdired)    ;; C-c C-q (C-c C-c finish)
(setq wdired-allow-to-change-permissions t)
(setq wdired-create-parent-directories t)
(bind-key "C-c C-q" 'wdired-change-to-wdired-mode dired-mode-map)

(setq dired-dwim-target t)

(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 2))

(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(bind-key (vector 'remap 'beginning-of-buffer) 'dired-back-to-top dired-mode-map)
(bind-key (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom dired-mode-map)

(put 'dired-find-alternate-file 'disabled nil)
(bind-key "RET" 'dired-find-alternate-file dired-mode-map)

(require 'dired-rainbow)

(defconst my-dired-media-files-extensions
  '("mp3" "mp4" "MP3" "MP4" "avi" "mpg" "mkv" "flv" "ogg")
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

(require 'dired-recent)
(dired-recent-mode 1)      ;; C-x C-d

(require 'dired-narrow)
(bind-key "/" 'dired-narrow dired-mode-map)   ;; g resets the view

;; (require 'dired-quick-sort)   ;; on osx: brew install coreutils + path
;; (dired-quick-sort-setup)      ;; binds "S" and invoke hydra
;; ;; (when *osx* (setq dired-use-ls-dired nil))

(defun open-file-external (file)
  (interactive "f")
  (let ((process-connection-type nil))
    (start-process
     "" nil shell-file-name
     shell-command-switch
     (format "nohup 1>/dev/null 2>/dev/null xdg-open %s"
             (expand-file-name file)))))

(defun dired-open ()
  (interactive)
  (let ((files (dired-get-marked-files nil nil)))
    (mapc (lambda (file) (open-file-external (shell-quote-argument file))) files)))

(bind-key (kbd "<C-return>") 'dired-open dired-mode-map)

(save-place-mode 1)
;; (desktop-save-mode 1)

(setq quelpa-update-melpa-p nil)

(require 'vc-darcs)
(add-to-list 'vc-handled-backends 'DARCS t)
(add-hook 'find-file-hooks 'vc-darcs-find-file-hook)

;; (if (file-exists-p "~/wren-mode.el")
;;     (add-to-list 'load-path "~/wren-mode.el")
;;   (quelpa '(wren-mode :fetcher github :repo "velkyel/wren-mode.el")))
;; (require 'wren-mode)

(require 'flycheck)

;; (flycheck-define-checker
;;  wren-lint
;;  "Wren syntax checker"
;;  :command ("wrenlint" source)
;;  :modes wren-mode
;;  :error-patterns ((error "WREN_ERROR_COMPILE in " (file-name) ":" line "> " (message) line-end)))

;; (add-hook 'wren-mode-hook (lambda ()
;;                             (message "activating wren-lint")
;;                             (flycheck-select-checker 'wren-lint)
;;                             (flycheck-mode)))


;; (quelpa '(completing-read-xref :fetcher github :repo "travitch/completing-read-xref.el"))
;; (require 'completing-read-xref)
;; (when (>= emacs-major-version 27)
;;   (setq xref-show-definitions-function #'completing-read-xref-show-defs))
;; (setq xref-show-xrefs-function #'completing-read-xref-show-xrefs)

(require 'helm-xref)
(setq xref-show-xrefs-function 'helm-xref-show-xrefs)

(require 'dumb-jump)
(setq dumb-jump-selector 'completing-read
      dumb-jump-prefer-searcher 'ag)

(add-to-list 'dumb-jump-language-file-exts '(:language "c++" :ext "mm" :agtype "cpp" :rgtype "cpp"))

(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq js-indent-level 2)

;; (quelpa '(inf-js :fetcher github :repo "velkyel/inf-js"))
;; (require 'inf-js)
;; (setq inf-js-program '("192.168.0.220" . 5555))
;; (add-hook 'js2-mode-hook 'inf-js-minor-mode)
;; (js2-imenu-extras-mode 1)

;; (require 'janet-mode)
;; (quelpa '(inf-janet :fetcher github :repo "velkyel/inf-janet"))
;; (require 'inf-janet)
;; (setq inf-janet-program '("192.168.0.220" . 5555))
;; (add-hook 'janet-mode-hook 'inf-janet-minor-mode)

;; (quelpa '(hlsl-mode :fetcher github :repo "darfink/hlsl-mode"))
;; (require 'hlsl-mode)
;; (add-to-list 'auto-mode-alist '("\\.hlsl\\'" . hlsl-mode))

(quelpa '(metal-mode :fetcher github :repo "masfj/metal-mode"))
(add-to-list 'auto-mode-alist '("\\.mtl\\'" . metal-mode))
(require 'metal-mode)

(require 'shader-mode)
(add-to-list 'auto-mode-alist '("\\.cginc$" . shader-mode))   ;; unity3d
(add-to-list 'auto-mode-alist '("\\.pix$" . shader-mode))     ;; g3d

(require 'magit)
(bind-key "C-c g" 'magit-status)
(add-to-list 'transient-values '(magit-pull "--rebase"))
;; (bind-key "C-x M-g" 'magit-dispatch-popup)

(bind-key "C-w" #'(lambda ()
                    (interactive)
                    (if (use-region-p)
                        (call-interactively 'kill-region)
                      (crux-kill-whole-line))))

(bind-key "C-a" 'crux-move-beginning-of-line)
(bind-key "C-c d" 'crux-duplicate-current-line-or-region)

(require 'whitespace)
(setq whitespace-line-column 90
      whitespace-style '(face trailing newline))

(require 'shrink-whitespace)
(bind-key "M-\\" 'shrink-whitespace)

(require 'expand-region)
(bind-key "M-=" 'er/expand-region)

(require 'visual-regexp)
(require 'visual-regexp-steroids)
(bind-key "C-c r" 'vr/replace)

(setq eldoc-idle-delay 0.2)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
(add-hook 'ielm-mode-hook 'eldoc-mode)

(require 'rainbow-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-mode)
(add-hook 'js2-mode-hook 'rainbow-mode)
(add-hook 'scheme-mode-hook 'rainbow-mode)
(add-hook 'janet-mode-hook 'rainbow-mode)
(add-hook 'lua-mode-hook 'rainbow-mode)

(require 'highlight-symbol)
(setq highlight-symbol-idle-delay 0.5)
(set-face-background 'highlight-symbol-face "gray78")

(defun symbol-replace (arg)    ;; C-u limit scope to current defun (defined by narrow-to-defun)
  (interactive "P")
  (save-excursion
    (let* ((oldsymbol (or (thing-at-point 'symbol)
                          (error "No symbol at point")))
           (newsymbol (query-replace-read-to
                       oldsymbol (format "%sReplace" (if arg "[function] " "")) nil))
           (counter 0))
      (if arg (goto-char (save-excursion (beginning-of-defun) (point)))
        ;; go to the beginning of the buffer..
        (goto-char (point-min)))
      (while (search-forward
              oldsymbol (if arg (save-excursion (end-of-defun) (point)) nil) t nil)
        (replace-match newsymbol nil t)
        (cl-incf counter 1))
      (message "Replaced %d matches" counter))))

(bind-key "M-'" 'symbol-replace)

(require 'smart-mark)
(smart-mark-mode)

(add-to-list 'auto-mode-alist '("\\.mm$" . objc-mode))
(add-to-list 'auto-mode-alist '("\\.\\(glsl\\|vert\\|frag\\|vsh\\|fsh\\|ush\\|usf\\|sc\\)\\'" . glsl-mode))
;; ...usf = unreal engine, sc = bgfx

(require 'google-translate)
;; fix https://github.com/atykhonov/google-translate/issues/137
(defun google-translate--search-tkk ()
  "Search TKK."
  (list 430675 2721866130))
(setq google-translate-default-source-language "en"
      google-translate-default-target-language "cs")

(add-to-list 'auto-mode-alist '("\\.p8$" . lua-mode))

(require 'lua-mode)
(add-hook 'lua-mode-hook 'flycheck-mode)
(setq lua-default-application '("192.168.0.122" . 5555))
;; (setq lua-default-application "lua5.3")  ;; '("localhost" . 5555))

(defun my-lua-switch-to-process-buffer ()
  (interactive)
  (when (lua-get-create-process)
    (switch-to-buffer-other-window lua-process-buffer)))

(bind-keys :map lua-mode-map
           ("C-M-x" . lua-send-defun)
           ("C-c C-b" . lua-send-buffer)
           ("C-c C-l" . lua-send-current-line)
           ("C-c C-z" . my-lua-switch-to-process-buffer))

(defun my-non-special-modes-setup ()
  (setq indicate-empty-lines t)
  (whitespace-mode)
  (goto-address-mode))

(defun my-prog-modes-hook ()
  (my-non-special-modes-setup)
  (make-local-variable 'comment-auto-fill-only-comments)
  (setq comment-auto-fill-only-comments t)
  (setq fill-column 90)
  (highlight-symbol-mode)
  (highlight-symbol-nav-mode)    ;; M-n, M-p
  (goto-address-prog-mode)
  (semantic-mode +1)     ;; for better imenu
  (bind-keys :map prog-mode-map
             ("C-." . helm-imenu)
             ("C->" . helm-imenu-anywhere)))

(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'text-mode-hook 'my-non-special-modes-setup)
(add-hook 'diff-mode-hook 'my-non-special-modes-setup)

(add-hook 'prog-mode-hook 'my-prog-modes-hook)

;; (add-hook 'c-mode-hook 'electric-pair-local-mode)
;; (add-hook 'c++-mode-hook 'electric-pair-local-mode)

(require 'smartparens-config)

(defun my-wrap-with-parens (&optional arg)
  (interactive "P")
  (sp-wrap-with-pair "("))

(defun my-wrap-with-double-quotes (&optional arg)
  (interactive "P")
  (sp-wrap-with-pair "\""))

(setq sp-navigate-reindent-after-up t)
(bind-keys :map smartparens-mode-map
           ("C-M-a" . sp-beginning-of-sexp)
           ("C-M-e" . sp-end-of-sexp)

           ("M-<down>" . sp-down-sexp)
           ("M-<up>"   . sp-up-sexp)
           ;; ("M-<down>" . sp-backward-down-sexp)
           ;; ("M-<up>"   . sp-backward-up-sexp)

           ("M-]" . sp-forward-sexp)
           ("M-[" . sp-backward-sexp)

           ("C-M-f" . sp-forward-sexp)
           ("C-M-b" . sp-backward-sexp)

           ("C-M-n" . sp-next-sexp)
           ("C-M-p" . sp-previous-sexp)

           ("C-M-t" . sp-transpose-sexp)
           ("C-M-k" . sp-kill-sexp)
           ("C-k"   . sp-kill-hybrid-sexp)
           ("M-k"   . sp-backward-kill-sexp)
           ("C-M-w" . sp-copy-sexp)
           ("C-M-d" . delete-sexp)

           ("C-c (" . my-wrap-with-parens)
           ("C-c \"" . my-wrap-with-double-quotes))

(add-hook 'clojure-mode-hook #'smartparens-mode)
(add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
(add-hook 'scheme-mode-hook #'smartparens-mode)
(add-hook 'lisp-mode-hook #'smartparens-mode)
(add-hook 'janet-mode-hook #'smartparens-mode)

(bind-key "C-M-;" 'comment-or-uncomment-sexp)

(require 'cmuscheme)

(defun scheme-send-buffer ()
  (interactive)
  (scheme-send-region (point-min) (point-max)))

(defun scheme-send-buffer-and-go ()
  (interactive)
  (scheme-send-buffer)
  (switch-to-buffer-other-window "*scheme*"))

(bind-keys :map scheme-mode-map
           ("C-c b" . scheme-send-buffer)
           ("C-c B" . scheme-send-buffer-and-go))

(put 'with-let 'scheme-indent-function 1)
(put 'with-baffle 'scheme-indent-function 0)
(put 'with-sound 'scheme-indent-function 1)
(put 'catch 'scheme-indent-function 1)
(put 'lambda* 'scheme-indent-function 1)
(put 'when 'scheme-indent-function 1)
(put 'let-temporarily 'scheme-indent-function 1)
(put 'let*-temporarily 'scheme-indent-function 1)
(put 'call-with-input-string 'scheme-indent-function 1)
(put 'unless 'scheme-indent-function 1)
(put 'letrec* 'scheme-indent-function 1)
(put 'sublet 'scheme-indent-function 1)
(put 'varlet 'scheme-indent-function 1)

(setq s7-host (if (string= (system-name) "Libors-Mac-mini.local")
                  "192.168.0.220"
                "localhost"))

(defun run-s7 ()
  (interactive)
  (setq scheme-program-name "s7")
  (if (not (comint-check-proc "*scheme*"))
      (let ((cmdlist (list `(,s7-host . 5555))))
        (set-buffer (apply 'make-comint "scheme" (car cmdlist) nil nil))
        (inferior-scheme-mode)))
  (setq scheme-buffer "*scheme*")
  (pop-to-buffer-same-window "*scheme*"))

(add-to-list 'auto-mode-alist '("\\.sld\\'" . scheme-mode))

(setq pulse-delay .06)

(require 'compile)
(bind-key "C-c C-t"
          #'(lambda ()
              (interactive)
              (compilation-set-skip-threshold (mod (1+ compilation-skip-threshold) 3)))
          compilation-mode-map)
(setq compile-command "ninja")

;; Avoid matching "from file:line:column:" as a warning.  For details
;; see
;; http://stackoverflow.com/questions/15489319/how-can-i-skip-in-file-included-from-in-emacs-c-compilation-mode
;; (setf (nth 5 (assoc 'gcc-include compilation-error-regexp-alist-alist)) 0)

(require 'find-file)
(nconc (cadr (assoc "\\.h\\'" cc-other-file-alist)) '(".m" ".mm"))
(add-to-list 'cc-other-file-alist '("\\.m\\'" (".h")))
(add-to-list 'cc-other-file-alist '("\\.mm\\'" (".h")))

(when (not *kelly*)
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
                                   " SortIncludes: false}")))

(setq clang-format-executable "clang-format")   ;; ev symlink in ~/bin

(require 'clang-format)
(fset 'c-indent-region 'clang-format-region)

(defun my-find-other-file ()
  (interactive)
  (ff-find-other-file nil t))

(bind-keys :map c-mode-base-map
           ("M-o" . my-find-other-file))

(bind-keys :map c++-mode-map
           ("C-M-\\" . clang-format-region)
           ("C-i" . clang-format))

(bind-keys :map c-mode-map
           ("C-M-\\" . clang-format-region)
           ("C-i" . clang-format))


(add-hook 'python-mode-hook
          (lambda ()
            (elpy-enable)
            (setq-local eldoc-mode nil)))

(with-eval-after-load 'python
  (setq elpy-rpc-python-command "python3")
  (setq elpy-formatter 'autopep8)
  (if *windows*
      (setq python-shell-interpreter "python.exe")
    (setq python-shell-interpreter "python3")))

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

(when (>= emacs-major-version 27)
  (setq isearch-lazy-count t))

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
(bind-key "M-r" #'(lambda ()
                    (interactive)
                    (call-interactively (if (get-buffer "*compilation*")
                                            'recompile
                                          'compile))))
(bind-key "M-R" 'shell-command)
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

(quelpa '(gnus-harvest :fetcher github :repo "jwiegley/gnus-harvest"))
(eval-after-load "gnus"
  '(progn (require 'gnus-harvest)
          (gnus-harvest-install)
          (bind-keys :map message-mode-map
                     ("C-M-i" . gnus-harvest-find-address))
          (require 'gnus-async)
          (setq gnus-asynchronous t)))

(quelpa '(gnus-article-treat-patch :fetcher github :repo "velkyel/gnus-article-treat-patch"))
(require 'gnus-article-treat-patch)
(setq ft/gnus-article-patch-conditions '( "^@@ -[0-9]+,[0-9]+ \\+[0-9]+,[0-9]+ @@" ))
(set-face-attribute 'ft/gnus-commit-message nil :foreground "black")
(set-face-attribute 'ft/gnus-diff-stat-file nil :foreground "black")
(set-face-attribute 'ft/gnus-diff-stat-bar nil :foreground "black")
(set-face-attribute 'ft/gnus-diff-stat-num nil :foreground "black")
(set-face-attribute 'ft/gnus-diff-misc nil :foreground "black")
(set-face-attribute 'ft/gnus-diff-hunk nil :inherit 'diff-hunk-header :foreground "black")

(setq gnus-select-method '(nnimap "fastmail"
                                  (nnimap-address "mail.messagingengine.com")
                                  (nnimap-server-port 993)
                                  (nnimap-stream ssl)
                                  (nnir-search-engine imap)
                                  (nnmail-expiry-wait immediate)
                                  ;; press E to expire mail
                                  (nnmail-expiry-target "nnimap+fastmail:INBOX.Trash"))
      gnus-permanently-visible-groups ".*\\(Inbox\\|INBOX\\).*"
      gnus-summary-line-format "%U%R%z %(%&user-date;  %-22,22f  %B%s%)\n"
      gnus-user-date-format-alist '((t . "%d-%m-%Y %H:%M"))
      gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date)  ;; gnus-thread-sort-by-date))
      gnus-message-archive-group "nnimap+fastmail:INBOX.Sent"
      gnus-gcc-mark-as-read t
      gnus-use-cache t
      ;; gnus-cache-enter-articles '(ticked dormant read unread)
      ;; gnus-cache-remove-articles nil
      ;; gnus-cacheable-groups "^nnimap"
      gnus-sum-thread-tree-false-root ""
      gnus-sum-thread-tree-indent " "
      gnus-sum-thread-tree-leaf-with-other "├► "
      gnus-sum-thread-tree-root ""
      gnus-sum-thread-tree-single-leaf "╰► "
      gnus-sum-thread-tree-vertical "│"
      gnus-use-full-window nil
      gnus-interactive-exit nil
      message-kill-buffer-on-exit t
      gnus-large-newsgroup nil
      gnus-read-active-file 'some
      mm-discouraged-alternatives '("text/html" "text/richtext")
      mm-text-html-renderer 'w3m
      gnus-inhibit-startup-message t
      gnus-agent nil
      ;; gnus-use-scoring nil
      gnus-parameters
      '((".*"
         (display . all))))

(defun exit-gnus-on-exit ()
  (if (and (fboundp 'gnus-group-exit)
           (gnus-alive-p))
      (with-current-buffer (get-buffer "*Group*")
        (let (gnus-interactive-exit)
          (gnus-group-exit)))))

(add-hook 'kill-emacs-hook 'exit-gnus-on-exit)

;; gnus-summary-mode
(eval-after-load 'gnus-sum
  '(progn
     (defhydra hydra-gnus-summary (:color blue)
       "
[_s_] Show thread   [_F_] Forward (C-c C-f)
[_h_] Hide thread   [_e_] Resend (S D e)
[_n_] Refresh (/ N) [_r_] Reply
[_!_] Mail -> disk  [_R_] Reply with original
[_d_] Disk -> mail  [_w_] Reply all (S w)
[_c_] Read all      [_W_] Reply all with original (S W)
[_#_] Mark          [_M_] Move to another folder
"
       ("s" gnus-summary-show-thread)
       ("h" gnus-summary-hide-thread)
       ("n" gnus-summary-insert-new-articles)
       ("F" gnus-summary-mail-forward)
       ("!" gnus-summary-tick-article-forward)
       ("d" gnus-summary-put-mark-as-read-next)
       ("c" gnus-summary-catchup-and-exit)
       ("e" gnus-summary-resend-message-edit)
       ("R" gnus-summary-reply-with-original)
       ("r" gnus-summary-reply)
       ("W" gnus-summary-wide-reply-with-original)
       ("w" gnus-summary-wide-reply)
       ("#" gnus-topic-mark-topic)
       ("M" gnus-summary-move-article)
       ("q" nil))
     ;; y is not used by default
     (define-key gnus-summary-mode-map "y" 'hydra-gnus-summary/body)))

;; gnus-article-mode
(eval-after-load 'gnus-art
  '(progn
     (defhydra hydra-gnus-article (:color blue)
       "
[_o_] Save attachment        [_F_] Forward
[_v_] Play video/audio       [_r_] Reply
[_d_] CLI to download stream [_R_] Reply with original
[_b_] Open external browser  [_w_] Reply all (S w)
[_f_] Click link/button      [_W_] Reply all with original (S W)
[_g_] Focus link/button
"
       ("F" gnus-summary-mail-forward)
       ("r" gnus-article-reply)
       ("R" gnus-article-reply-with-original)
       ("w" gnus-article-wide-reply)
       ("W" gnus-article-wide-reply-with-original)
       ("o" gnus-mime-save-part)
       ("v" w3mext-open-with-mplayer)
       ("d" w3mext-download-rss-stream)
       ("b" w3mext-open-link-or-image-or-url)
       ("f" w3m-lnum-follow)
       ("g" w3m-lnum-goto)
       ("q" nil))
     ;; y is not used by default
     (define-key gnus-article-mode-map "y" 'hydra-gnus-article/body)))

;; message-mode
(eval-after-load 'message
  '(progn
     (defhydra hydra-message (:color blue)
  "
[_c_] Complete mail address
[_a_] Attach file
[_s_] Send mail (C-c C-c)
"
       ("c" gnus-harvest-find-address)
       ("a" mml-attach-file)
       ("s" message-send-and-exit)
       ("q" nil))))

(defun message-mode-hook-hydra-setup ()
  (local-set-key (kbd "C-c C-y") 'hydra-message/body))
(add-hook 'message-mode-hook 'message-mode-hook-hydra-setup)

;; (when *osx*
;;   (add-to-list 'load-path "/usr/local/opt/mu/share/emacs/site-lisp/mu/mu4e"))

;; (when *linux*
;;   (add-to-list 'load-path "~/mu/mu4e"))

;; (require 'mu4e)

;; (defun mu4e-display-image (imgpath &optional maxwidth maxheight)
;;   "Display image IMG at point; optionally specify MAXWIDTH and MAXHEIGHT."
;;   (let ((img (create-image imgpath nil nil
;;                            :max-width maxwidth :max-height maxheight)))
;;     (save-excursion
;;       (insert "\n")
;;       (let ((size (image-size img))) ;; inspired by gnus..
;;         (insert-char ?\n
;;                      (max 0 (round (- (window-height) (or maxheight (cdr size)) 1) 2)))
;;         (insert-char ?\.
;;                      (max 0 (round (- (window-width)  (or maxwidth (car size))) 2)))
;;         (insert-image img)))))

;; (setq mail-user-agent 'mu4e-user-agent
;;       mu4e-attachment-dir "~/Downloads"
;;       mu4e-root-maildir (expand-file-name "~/Maildir")
;;       mu4e-drafts-folder "/INBOX.Drafts"
;;       mu4e-sent-folder "/INBOX.Sent"
;;       mu4e-trash-folder "/INBOX.Trash"
;;       mu4e-get-mail-command "offlineimap -o"
;;       mu4e-update-interval nil ;; 300
;;       mu4e-confirm-quit nil
;;       mu4e-date-format-long "%d.%m.%Y"
;;       mu4e-headers-date-format "%d.%m.%y"
;;       mu4e-view-show-addresses t
;;       mu4e-sent-messages-behavior 'sent
;;       mu4e-view-show-images t
;;       mu4e-completing-read-function #'completing-read
;;       mu4e-compose-signature-auto-include nil
;;       mu4e-headers-leave-behavior 'apply
;;       mu4e-html2text-command "w3m -I UTF-8 -O UTF-8 -dump -T text/html"  ;; "html2text -utf8 -width 72"
;;       message-kill-buffer-on-exit t
;;       mu4e-maildir-shortcuts
;;       '(("/INBOX" . ?i)
;;         ("/INBOX.Sent" . ?s)
;;         ("/INBOX.Trash" . ?t)
;;         ("/INBOX.Archive" . ?a))
;;       mu4e-bookmarks '(("flag:unread AND NOT flag:trashed AND NOT maildir:/INBOX.Trash AND NOT maildir:/INBOX.Spam AND NOT maildir:/INBOX.Sent" "Unread messages" ?u)
;;                        ("date:today..now AND NOT maildir:/INBOX.Trash AND NOT maildir:/INBOX.Spam AND NOT maildir:/INBOX.Sent" "Today's messages" ?t)
;;                        ("date:7d..now AND NOT maildir:/INBOX.Trash AND NOT maildir:/INBOX.Spam" "Last 7 days" ?w)
;;                        ("mime:image/*" "Messages with images" ?p)
;;                        ("size:2M..500M" "Big messages" ?b)))

;; ;; (quelpa '(mu4e-patch :fetcher github :repo "seanfarley/mu4e-patch"))
;; ;; (require 'mu4e-patch)
;; ;; (add-hook 'mu4e-view-mode-hook #'mu4e-patch-highlight)
;; ;; (copy-face 'mu4e-header-key-face 'mu4e-patch-commit-message)

;; ;; https://rakhim.org/fastmail-setup-with-emacs-mu4e-and-mbsync-on-macos/
;; (fset 'my-move-to-trash "mt")
;; (define-key mu4e-headers-mode-map (kbd "d") 'my-move-to-trash)
;; (define-key mu4e-view-mode-map (kbd "d") 'my-move-to-trash)

;; (global-set-key (kbd "C-c m") 'mu4e)

(defun my-org-mode-setup ()
  (org-bullets-mode)
  (setq fill-column 100))

(require 'org)
(require 'org-bullets)
(add-hook 'org-mode-hook 'my-org-mode-setup)
(setq org-clock-into-drawer "CLOCKING")

(require 'helm-org-rifle)
(bind-key (kbd "C-.") 'helm-org-rifle-current-buffer org-mode-map)

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

(set-face-attribute 'helm-selection
                    nil
                    :background "#a5e8be")

(set-face-attribute 'helm-visible-mark
                    nil
                    :background "#f1c40f")

(set-face-attribute 'helm-ff-executable
                    nil
                    :foreground "#228b22")

(set-face-attribute 'helm-source-header
                    nil
                    :height 1.0)

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

(set-face-attribute 'dired-subtree-depth-1-face nil :background "gray85")
(set-face-attribute 'dired-subtree-depth-2-face nil :background "gray85")
(set-face-attribute 'dired-subtree-depth-3-face nil :background "gray85")
(set-face-attribute 'dired-subtree-depth-4-face nil :background "gray85")
(set-face-attribute 'dired-subtree-depth-5-face nil :background "gray85")
(set-face-attribute 'dired-subtree-depth-6-face nil :background "gray85")

(with-eval-after-load 'popup
  (set-face-attribute 'popup-face
                      nil
                      :background "#dddd30"))

(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file :noerror)
