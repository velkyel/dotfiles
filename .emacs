
(setq compilation-ask-about-save nil
      compilation-scroll-output 'first-error
      next-line-add-newlines nil
      inhibit-startup-message t
      initial-scratch-message nil
      font-lock-maximum-decoration '((racket-mode . t) (t . 1))
      load-prefer-newer t
      require-final-newline t
      redisplay-dont-pause t
      column-number-mode t
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
(iswitchb-mode t)   ;; substring buffer switch
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default py-indent-offset 4)
(setq-default abbrev-mode t)
(winner-mode t)   ;; C-c <left|right>
(set-language-environment "czech")
(setq default-input-method "czech-qwerty")
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(defalias 'yes-or-no-p 'y-or-n-p)

;(setq-default line-spacing nil)
(setq global-auto-revert-non-file-buffers t)
;; (setq auto-revert-verbose nil)
(global-auto-revert-mode 1)

(require 'em-alias)
(setq eshell-command-aliases-list (append '(("l" "ls -lcrt")
                                            ("d" "dired $1")
                                            ("ff" "find-file $1"))
                                          eshell-command-aliases-list))

(defun visit-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (if (not (get-buffer "*eshell*"))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (eshell))
    (switch-to-buffer-other-window "*eshell*")))

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
    (setq fill-column 100)
    ;; (setq c-hungry-delete-key t)
    ))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; (require 'python)
;(setq python-python-command "python3")

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

(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(setq package-pinned-packages
      '((cider          . "melpa-stable")
        (clojure-mode   . "melpa-stable")))

(package-initialize)

(defvar df/packages '(clojure-mode
                      pixie-mode
                      lua-mode
                      expand-region
                      vc-darcs
                      ninja-mode
                      ag
                      avy
                      google-translate
                      wgrep
                      wgrep-ag
                      mwim
                      exec-path-from-shell
                      rust-mode
                      swift-mode
                      elpy
                      define-word
                      imenu-anywhere
                      rainbow-mode
                      flatui-theme
                      json-mode
                      undo-tree
                      racket-mode
                      processing-mode
                      csharp-mode
                      diminish
                      irony
                      ;; company
                      ;; company-irony
                      cider
                      shrink-whitespace
                      browse-kill-ring
                      nyan-mode
                      highlight-parentheses
                      projectile
                      glsl-mode))

(if (not (file-directory-p package-user-dir))     ;; jinak chci manualne
    (package-refresh-contents))

(dolist (p df/packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'projectile)
(projectile-global-mode)

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

(require 'browse-kill-ring)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)

(require 'diminish)
(after 'undo-tree (diminish 'undo-tree-mode))
(after 'rainbow-mode (diminish 'rainbow-mode))
(after 'abbrev (diminish 'abbrev-mode))
(after 'whitespace (diminish 'whitespace-mode))
(after 'highlight-parentheses (diminish 'highlight-parentheses-mode))

(require 'nyan-mode)
(nyan-mode 1)
(setq nyan-bar-length 16
      naya-wavy-trail t)

(require 'whitespace)
(setq whitespace-line-column 100)             ;; limit line length
(setq whitespace-style '(face trailing newline))
(add-hook 'prog-mode-hook 'whitespace-mode)

(elpy-enable)
(when (or (equal system-type 'darwin) (kelly?))
  (setq elpy-modules (delete 'elpy-module-flymake elpy-modules)))
;; ...nepodarilo se mi zatim zprovoznit

(add-hook 'pixie-mode-hook #'inf-clojure-minor-mode)
(require 'expand-region)

(require 'highlight-parentheses)
(require 'rainbow-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-mode)
(add-hook 'emacs-lisp-mode-hook 'highlight-parentheses-mode)
(require 'json-mode)
(require 'define-word)
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(FIXME\\|TODO\\):" 1 font-lock-preprocessor-face prepend)))))

(require 'imenu-anywhere)

(require 'undo-tree)
(global-undo-tree-mode 1)           ;; C-x u
(defalias 'redo 'undo-tree-redo)

(if (kelly?)
    (set-background-color "gray90")
  (load-theme 'flatui t))

(exec-path-from-shell-initialize)

(setq vc-disable-async-diff nil)        ;; hotfix: vc-darcs je modul o ktery se zjevne nikdo moc nestara
(add-to-list 'vc-handled-backends 'DARCS)
(autoload 'vc-darcs-find-file-hook "vc-darcs")
(add-hook 'find-file-hooks 'vc-darcs-find-file-hook)

(require 'avy)
(require 'google-translate)
(require 'google-translate-default-ui)
;; (require 'google-translate-smooth-ui)

(require 'wgrep)
(require 'wgrep-ag)

(require 'mwim)

(autoload 'glsl-mode "glsl-mode" nil t)

(setq auto-mode-alist (append '(("\\.mm?$" . objc-mode)
                                ("\\.as?$" . js-mode)
                                ("\\.glsl\\'" . glsl-mode)
                                ("\\.vert\\'" . glsl-mode)
                                ("\\.frag\\'" . glsl-mode)
                                ("\\.vsh\\'" . glsl-mode)
                                ("\\.fsh\\'" . glsl-mode)
                                ("\\.usf\\'" . glsl-mode))     ;; unreal engine
                              auto-mode-alist))

(defun setup-mu4e ()
  (setq epa-file-cache-passphrase-for-symmetric-encryption t)
  (setq smtpmail-auth-credentials "~/.authinfo")
  (when (equal system-type 'gnu/linux)
    (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e"))
  (when (equal system-type 'darwin)
    (setq mu4e-mu-binary "/usr/local/bin/mu")
    (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e"))

  (require 'mu4e)

  (when (equal system-type 'gnu/linux)
    (setq mu4e-mu-binary "/usr/bin/mu"))

  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  (setq mu4e-maildir "~/Maildir"
        mu4e-drafts-folder "/[Gmail].Drafts"
        mu4e-sent-folder   "/[Gmail].Sent Mail"
        mu4e-trash-folder  "/[Gmail].Trash"
        mu4e-maildir-shortcuts
        '( ("/INBOX"             . ?i)
           ("/[Gmail].Sent Mail" . ?s)
           ("/[Gmail].Trash"     . ?t)
           ("/[Gmail].All Mail"  . ?a))
        mu4e-bookmarks '( ("flag:unread AND NOT flag:trashed AND NOT maildir:/[Gmail].Trash AND NOT maildir:/[Gmail].Spam" "Unread messages"      ?u)
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
        mu4e-confirm-quit nil)
  ;; (setq mu4e-use-fancy-chars t)

  (set-face-attribute 'mu4e-unread-face nil
                      :inherit font-lock-preprocessor-face
                      :bold t)

  (require 'smtpmail)

  ;; (defadvice smtpmail-send-it (around fix-using-openssl activate)
  ;;   (let ((tls-program
  ;;          '("openssl s_client -connect %h:%p -no_ssl2 -ign_eof -starttls smtp"))
  ;;         tls-program)
  ;;     ad-do-it))

  (setq message-send-mail-function 'smtpmail-send-it
        ;;smtpmail-stream-type 'ssl
        starttls-use-gnutls t
        smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
        smtpmail-auth-credentials '(("smtp.gmail.com" 587 "capak44@gmail.com" nil))
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587)

  (setq message-kill-buffer-on-exit t)

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

  (global-set-key (kbd "C-c m") '(lambda ()
                                   (interactive)
                                   (mu4e)
                                   (mu4e-update-mail-and-index nil))))

(defun setup-cider ()
  (require 'cider)
  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
  ;; (setq cider-auto-mode nil)
  ;; (setq nrepl-log-messages nil)
  ;; (setq nrepl-hide-special-buffers t)
  (setq cider-repl-use-pretty-printing t)
  (setq cider-prompt-save-file-on-load nil))

(when (not (kelly?))
  (setq compile-command "scons")
  (setup-mu4e)
  (setup-cider)
  (require 'racket-mode)
  (add-hook 'racket-mode-hook          ;; same as C-c C-k
            (lambda ()
              (define-key racket-mode-map (kbd "C-c r") 'racket-run))))

(define-key function-key-map "\e[$" (kbd "C-$"))
(define-key function-key-map "\e[%" (kbd "C-%"))
(define-key function-key-map "\e[," (kbd "C-,"))
(define-key function-key-map "\e[;" (kbd "C-;"))
(define-key function-key-map "\e[=" (kbd "C-="))
(define-key function-key-map "\e[." (kbd "C-."))

(define-key (current-global-map) [remap yank-pop] 'browse-kill-ring)   ;; remap yank-pop
(after 'browse-kill-ring (setq browse-kill-ring-replace-yank t))

(define-key global-map (kbd "RET") 'newline-and-indent)
;; (define-key c-mode-map (kbd "TAB") 'company-indent-or-complete-common)
;; (define-key c++-mode-map (kbd "TAB") 'company-indent-or-complete-common)
(global-set-key (kbd "M-r") 'recompile)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-c t") 'visit-term-buffer)
(global-set-key (kbd "C-c C-f") 'projectile-find-file)

(global-set-key (kbd "C-;") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "M-g l") 'avy-goto-line)
(after 'isearch (define-key isearch-mode-map (kbd "C-;") 'avy-isearch))

(global-set-key (kbd "C-a") 'mwim-beginning-of-code-or-line)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
;; (global-set-key (kbd "C-e") 'mwim-end-of-code-or-line)
(global-set-key (kbd "C-.") 'imenu-anywhere)
(global-set-key (kbd "M-\\") 'shrink-whitespace)
(global-set-key (kbd "M-o") 'other-window)
