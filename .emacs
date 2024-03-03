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

(setq inhibit-startup-message t
      frame-inhibit-implied-resize t
      initial-scratch-message nil)

(setq package-list '(bind-key
                     auto-package-update
                     auto-compile
                     exec-path-from-shell
                     visual-regexp
                     json-mode
                     restart-emacs
                     vertico
                     orderless
                     consult
                     embark-consult
                     deadgrep
                     projectile
                     super-save
                     avy
                     goto-chg
                     unfill
                     quelpa
                     ninja-mode
                     lua-mode
                     expand-region
                     rainbow-mode
                     smart-mark
                     google-translate
                     glsl-mode
                     clang-format
                     highlight-symbol
                     magit
                     volatile-highlights
                     key-seq
                     dumb-jump
                     shackle
                     crux
                     web-mode
                     js2-mode
                     dired-subtree
                     dired-rainbow
                     dired-recent
                     dired-narrow
                     markdown-mode
                     org-superstar
                     smartparens
                     vc-darcs
                     persistent-scratch
                     comment-or-uncomment-sexp
                     minions
                     hydra
                     restclient
                     lsp-mode      ;; needs clangd package
                     ))

(set-language-environment "czech")
(setq default-input-method "czech-qwerty")
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'quelpa)

(require 'auto-package-update)
(setq auto-package-update-delete-old-versions t)
(auto-package-update-maybe)
(add-hook 'auto-package-update-before-hook
          (lambda () (message "I will update packages now") (quelpa-upgrade-all)))

(require 'auto-compile)
(auto-compile-on-load-mode 1)
(auto-compile-on-save-mode 1)

(require 'visual-regexp)
(defalias 'replace-regexp 'vr/replace)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)

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
      use-dialog-box nil
      shift-select-mode nil
      uniquify-buffer-name-style 'forward
      ediff-window-setup-function 'ediff-setup-windows-plain
      search-highlight t
      isearch-allow-scroll t
      isearch-lazy-count t
      search-ring-max 200
      regexp-search-ring-max 200
      eval-expression-print-level nil
      mail-user-agent 'gnus-user-agent
      user-mail-address "capak@inputwish.com"
      user-full-name  "Libor Čapák"
      scroll-margin 4
      scroll-conservatively 101
      vc-follow-symlinks t
      find-file-visit-truename t
      calendar-week-start-day 1
      confirm-kill-processes nil
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

(delete-selection-mode t)
(setq show-paren-delay 0)   ;; must be set before mode activating
(show-paren-mode 1)
(transient-mark-mode t)
(which-function-mode)
(winner-mode t)   ;; C-c <left|right>
(defalias 'yes-or-no-p 'y-or-n-p)
(windmove-default-keybindings)
(savehist-mode 1)

(global-so-long-mode 1)

(persistent-scratch-setup-default)

(require 'crux)

(setenv "PAGER" (executable-find "cat"))

(defun shell-here ()
  "Opens up a new shell in the directory associated with the
    current buffer's file. The eshell is renamed to match that
    directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (name (car (last (split-string parent "/" t)))))
    ;; (other-window 1)
    (shell (concat "*shell: " name "*"))))
;; TODO: switch to only if exists

(global-set-key (kbd "C-c t") 'shell-here)

;; (bind-key "C-c t" #'crux-visit-shell-buffer)

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
(setq recentf-max-saved-items 100)
;; (add-to-list 'recentf-exclude "bookmarks")

(when *osx*
  ;; (setq mac-command-modifier 'meta)
  ;; (setq mac-option-modifier nil)
  (setq mac-option-modifier 'nil)
  (setq mac-command-modifier 'meta)
  (setq ns-function-modifier 'hyper)
  (add-to-list 'default-frame-alist '(font . "hack 14"))
  ;; brew install coreutils:
  (setq insert-directory-program "gls" dired-use-ls-dired t)
  (setq dired-listing-switches "-al --group-directories-first"))

(when *windows*
  (add-to-list 'default-frame-alist '(font . "hack 11")))

(when (and (display-graphic-p) *linux*)
  (add-to-list 'default-frame-alist '(font . "hack 11")))

(require 'goto-chg)
(bind-key "C-x C-\\" 'goto-last-change)

(when (or *osx* *linux*)
  (require 'exec-path-from-shell)
  (setq exec-path-from-shell-arguments (remove "-i" exec-path-from-shell-arguments))   ;; optimization
  (exec-path-from-shell-initialize))

(setq python-shell-completion-native-enable nil)

(require 'vertico)
(vertico-mode 1)
(setq vertico-count 15)

(require 'orderless)
(setq completion-styles '(orderless)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))

(require 'consult)
(setq consult-preview-key nil)
(bind-key "C-x b" 'consult-buffer)
(setq xref-show-xrefs-function 'consult-xref
      xref-show-definitions-function 'consult-xref)

(consult-customize consult-buffer consult-xref consult-imenu consult-imenu-multi :group nil)

(bind-key "M-y" 'consult-yank-pop)

(require 'embark-consult)
(define-key minibuffer-local-map (kbd "C-x s") 'embark-export)
(define-key minibuffer-local-map (kbd "C-x C-s") 'embark-export)

(require 'projectile)
(setq projectile-indexing-method 'native
      projectile-enable-caching t
      projectile-completion-system 'default)
(projectile-global-mode)
(bind-key "C-x C-p" 'projectile-find-file)
(setq consult-project-root-function #'projectile-project-root)

(defconst fg/consult--source-git-ls-files
  `(:name     "Git ls-files"
    :narrow   (?g . "Git ls-files")
    :category file
    :face     consult-file
    :history  file-name-history
    :state    ,#'consult--file-state
    :enabled  ,(lambda () (and consult-project-root-function))
    :items
    ,(lambda ()
      (when-let (root (consult--project-root))
	(let* ((default-directory root)
	       (cmd (format "git ls-files --full-name"))
	       (files (split-string (shell-command-to-string cmd) "\n" t))
	       (abs-files (mapcar (lambda (fn) (expand-file-name fn root)) files))
	       )
	  abs-files))))
  "Git ls-files candidate source for `consult-buffer'.")

(add-to-list 'consult-buffer-sources fg/consult--source-git-ls-files t)

(require 'grep)
(add-to-list 'grep-find-ignored-files ".DS_Store")
(add-to-list 'grep-find-ignored-files "TAGS")
(add-to-list 'grep-find-ignored-directories ".build")
(add-to-list 'grep-find-ignored-directories "build")
(add-to-list 'grep-find-ignored-directories "_darcs")

;; (defun my/project-ripgrep ()
;;   (interactive)
;;   (consult-ripgrep (projectile-project-root) (thing-at-point 'symbol)))

;; (defun my/directory-ripgrep ()
;;   (interactive)
;;   (consult-ripgrep default-directory (thing-at-point 'symbol)))

;; (consult-customize my/project-ripgrep :group nil)
;; (consult-customize my/directory-ripgrep :group nil)

;; (bind-key "M-g" 'my/project-ripgrep)
;; (bind-key* "M-G" 'my/directory-ripgrep)

(require 'deadgrep)
(bind-key "M-g" 'deadgrep)

(require 'shackle)
(setq shackle-rules
      '(("*Help*" :align t :select t)
        (compilation-mode :other t)
        ((inferior-scheme-mode "*shell*" "*eshell*") :popup t))
      ;; shackle-default-rule '(:select t)
      shackle-default-size 0.4
      shackle-inhibit-window-quit-on-same-windows t)
(shackle-mode)

;; https://www.reddit.com/r/emacs/comments/pavjxj/how_to_make_compilegotoerror_use_the_current/

(defun display-buffer-from-compilation-p (_buffer-name _action)
  (unless current-prefix-arg
    (with-current-buffer (window-buffer)
      (derived-mode-p 'compilation-mode))))

(push '(display-buffer-from-compilation-p
        display-buffer-same-window
        (inhibit-same-window . nil))
      display-buffer-alist)

(require 'json-mode)    ;; C-c C-p show-path; C-c C-f beautify
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

(require 'super-save)
(super-save-mode 1)

(require 'avy)
(setq avy-background t)

(require 'key-seq)
(key-seq-define-global "jj" 'avy-goto-word-or-subword-1)
(key-seq-define-global "jl" 'goto-line)
(key-seq-define-global "JJ" 'crux-switch-to-previous-buffer)
(key-seq-define-global "qq" 'bookmark-jump)
(key-seq-define-global "QQ" 'bookmark-set-no-overwrite)

(key-chord-mode +1)
(setq key-chord-safety-interval-forward 0.1)

(require 'unfill)
(bind-key [remap fill-paragraph] 'unfill-toggle)

(require 'dired)
(require 'dired-subtree)
(bind-key "<tab>" 'dired-subtree-toggle dired-mode-map)

(setq dired-recursive-copies 'always
      dired-recursive-deletes 'always
      dired-isearch-filenames 'dwim)

(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(eval-after-load "dired-aux"
   '(add-to-list 'dired-compress-file-suffixes '("\\.zip\\'" ".zip" "unzip")))

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
  '("mp3" "mp4" "MP3" "MP4" "avi" "mpg" "mkv" "mov" "flv" "ogg" "m4v" "mpeg")
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

(setq quelpa-update-melpa-p nil)

(require 'vc-darcs)
(add-to-list 'vc-handled-backends 'DARCS t)
(add-hook 'find-file-hooks 'vc-darcs-find-file-hook)

(require 'dumb-jump)
(setq dumb-jump-selector 'completing-read
      dumb-jump-prefer-searcher 'rg)

(add-to-list 'dumb-jump-language-file-exts '(:language "c++" :ext "mm" :agtype "cpp" :rgtype "cpp"))

(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq js-indent-level 2)

(quelpa '(inf-js :fetcher github :repo "velkyel/inf-js"))
(require 'inf-js)
(setq inf-js-program '("192.168.0.122" . 5555))
(add-hook 'js2-mode-hook 'inf-js-minor-mode)
(js2-imenu-extras-mode 1)

(quelpa '(metal-mode :fetcher github :repo "masfj/metal-mode"))
(add-to-list 'auto-mode-alist '("\\.mtl\\'" . metal-mode))
(require 'metal-mode)


(quelpa '(modeline-posn :fetcher url :url "https://www.emacswiki.org/emacs/download/modeline-posn.el"))
(require 'modeline-posn)
(size-indication-mode 1)

(require 'magit)
(bind-key "C-c g" 'magit-status)
(add-to-list 'transient-values '(magit-pull "--rebase"))

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

(require 'expand-region)
(bind-key "M-=" 'er/expand-region)

(setq eldoc-idle-delay 0.2)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
(add-hook 'ielm-mode-hook 'eldoc-mode)

(require 'rainbow-mode)

; hex colors only:
(setq rainbow-html-colors nil)
(setq rainbow-x-colors nil)
(setq rainbow-latex-colors nil)
(setq rainbow-r-colors nil)

(add-hook 'emacs-lisp-mode-hook 'rainbow-mode)
(add-hook 'js2-mode-hook 'rainbow-mode)
(add-hook 'scheme-mode-hook 'rainbow-mode)
(add-hook 'lua-mode-hook 'rainbow-mode)
(add-hook 'org-mode-hook 'rainbow-mode)

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
(require 'facemenu)    ;; fix google-translate dependency
;; fix https://github.com/atykhonov/google-translate/issues/137
(defun google-translate--search-tkk ()
  "Search TKK."
  (list 430675 2721866130))
(setq google-translate-default-source-language "en"
      google-translate-default-target-language "cs")

(require 'lua-mode)
(add-to-list 'auto-mode-alist '("\\.p8$" . lua-mode))
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
  (setq fill-column 100)
  (highlight-symbol-mode)
  (highlight-symbol-nav-mode)    ;; M-n, M-p
  (goto-address-prog-mode)
  (bind-keys :map prog-mode-map
             ("C-." . consult-imenu)
             ("C->" . consult-imenu-multi)))

(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'text-mode-hook 'my-non-special-modes-setup)
(add-hook 'diff-mode-hook 'my-non-special-modes-setup)

(add-hook 'prog-mode-hook 'my-prog-modes-hook)

(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
(setq lsp-warn-no-matched-clients nil
      lsp-completion-provider :none
      lsp-headerline-breadcrumb-enable nil
      lsp-diagnostics-provider :none
      lsp-enable-snippet nil
      lsp-enable-symbol-highlighting nil
      lsp-completion-enable nil
      lsp-enable-links nil
      lsp-enable-dap-auto-configure nil
      lsp-eldoc-enable-hover nil
      lsp-enable-on-type-formatting nil
      lsp-before-save-edits nil
      lsp-modeline-diagnostics-enable nil
      lsp-modeline-workspace-status-enable nil
      lsp-lens-enable nil)

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

(add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
(add-hook 'scheme-mode-hook #'smartparens-mode)
(add-hook 'lisp-mode-hook #'smartparens-mode)

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
(put 'dotimes 'scheme-indent-function 1)

(setq s7-host (if (string= (system-name) "Libors-Mac-mini.local")
                  "169.254.73.255"
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

(setq clang-format-style (concat "{BasedOnStyle: Google,"
                                 " BreakBeforeBraces: Mozilla,"
                                 " BinPackParameters: true,"
                                 " BreakBeforeBinaryOperators: NonAssignment,"
                                 " IndentWidth: 2,"
                                 " ColumnLimit: 100,"
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
(fset 'c-indent-line-or-region 'clang-format-region)

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
(bind-key "M-r" #'(lambda ()
                    (interactive)
                    (call-interactively (if (get-buffer "*compilation*")
                                            'recompile
                                          'compile))))

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
[_u_] Clear mark
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
       ("u" gnus-summary-clear-mark-forward)
       ("q" nil))
     ;; y is not used by default
     (define-key gnus-summary-mode-map "y" 'hydra-gnus-summary/body)))

;; gnus-article-mode
(eval-after-load 'gnus-art
  '(progn
     (defhydra hydra-gnus-article (:color blue)
       "
[_o_] Save attachment        [_F_] Forward
[_r_] Reply                  [_R_] Reply with original
[_w_] Reply all (S w)        [_W_] Reply all with original (S W)

"
       ("F" gnus-summary-mail-forward)
       ("r" gnus-article-reply)
       ("R" gnus-article-reply-with-original)
       ("w" gnus-article-wide-reply)
       ("W" gnus-article-wide-reply-with-original)
       ("o" gnus-mime-save-part)
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

(defun my-org-mode-setup ()
  (org-superstar-mode 1)
  (setq org-startup-with-inline-images t)
  (hl-line-mode 1)
  (setq fill-column 100))

(require 'org)
(require 'org-superstar)
(add-hook 'org-mode-hook 'my-org-mode-setup)
(setq org-clock-into-drawer "CLOCKING")
(setq org-todo-keywords '((sequence "TODO" "WAITING" "|" "CANCELLED" "DONE")))

(bind-key (kbd "C-.") 'consult-org-heading org-mode-map)

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

(with-eval-after-load 'highlight-indentation
  (set-face-attribute 'highlight-indentation-face
                     nil
                     :background "gray80"))

(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file :noerror)
