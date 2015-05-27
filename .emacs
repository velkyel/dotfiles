
;; (setq compilation-skip-threshold 2)
(setq compilation-ask-about-save nil)    ;; autosave on compile and friends

;; (setq x-alt-keysym 'meta)

(delete-selection-mode +1)

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "M-r") 'recompile)
(setq compile-command "scons")

(setq next-line-add-newlines nil)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(show-paren-mode 1)
(transient-mark-mode t)
(menu-bar-mode -1)
(if window-system (tool-bar-mode -1))
;(if window-system (tabbar-mode -1))
(if window-system (scroll-bar-mode -1))
(if window-system (fringe-mode 2))
(which-function-mode)
(column-number-mode)
(iswitchb-mode t)   ;; substring buffer switch
(setq-default indent-tabs-mode nil)  ;; nevklada tabs ale white spaces
(setq-default tab-width 4)
(winner-mode t)   ;; C-c <left|right>
(setq font-lock-maximum-decoration 1)
(setq vc-diff-switches "-u")
(set-language-environment "czech")
(setq default-input-method "czech-qwerty")
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq fill-column 100)
;; (fset 'yes-or-no-p 'y-or-n-p)

;; (set-background-color "gray90")

;; (progn
;;   (set-face-attribute 'cursor nil
;;                       :background "#EC407A"
;;                       :inverse-video t)
;;   (set-face-attribute 'default nil
;;                       :inherit nil
;;                       :background "#EAEAEA"
;;                       :foreground "#3f525b"))   ;; (color-darken-name "#546E7A" 10)))))

(require 'whitespace)
(setq whitespace-line-column 100)                       ;; limit line length
(setq whitespace-style '(face trailing lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

;(setq-default line-spacing nil)
(global-auto-revert-mode 1)

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

(setq auto-mode-alist (append '(("\\.mm?$" . objc-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.json?$" . js-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.as?$" . js-mode)) auto-mode-alist))

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
  (font-lock-add-keywords
   nil
   '((my-c-mode-font-lock-if0 (0 font-lock-comment-face prepend))) 'add-to-end))

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
  (set-default-font "Inconsolata-18"))

(when (and window-system (equal system-type 'gnu/linux))
  (set-default-font "Inconsolata-15"))

;;osx keys
;; (setq mac-option-key-is-meta nil)
;; (setq mac-command-key-is-meta t)
;; (setq mac-command-modifier 'meta)
;; (setq mac-option-modifier 'alt)

(require 'package)
;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(defvar df/packages '(clojure-mode
                      pixie-mode
                      lua-mode
                      expand-region
                      sublime-themes
                      vc-darcs
                      ninja-mode
                      ag
                      ace-jump-mode
                      ace-jump-buffer
                      google-translate
                      wgrep
                      wgrep-ag
                      mwim
                      exec-path-from-shell
                      rust-mode
                      swift-mode
                      greymatters-theme
                      elpy
                      define-word
                      repl-toggle
                      sx
                      flatui-theme
                      ;; swiper
                      glsl-mode))

(dolist (p df/packages)
  (when (not (package-installed-p p))
    (package-install p)))

(elpy-enable)
(when (equal system-type 'darwin)
  (setq elpy-modules (delete 'elpy-module-flymake elpy-modules)))
;; ...nepodarilo se mi zatim zprovoznit

(add-hook 'pixie-mode-hook #'inf-clojure-minor-mode)
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(require 'define-word)
(require 'repl-toggle)
(require 'sx)

;; (require 'swiper)
;; (global-set-key (kbd "C-s") 'swiper)

;; (unless (package-installed-p 'nrepl)
;;   (package-install 'nrepl))

(exec-path-from-shell-initialize)

(setq vc-disable-async-diff nil)
;; hotfix: vc-darcs je modul o ktery se zjevne nikdo moc nestara
(add-to-list 'vc-handled-backends 'DARCS)
(autoload 'vc-darcs-find-file-hook "vc-darcs")
(add-hook 'find-file-hooks 'vc-darcs-find-file-hook)

;; (unless (package-installed-p 'sml-modeline)
;;   (package-install 'sml-modeline))
;; (sml-modeline-mode 1)
;; (set-face-background 'sml-modeline-end-face "LightYellow3")
;; (set-face-background 'sml-modeline-vis-face "LightBlue3")
;; (setq sml-modeline-len 16)

;; (unless (package-installed-p 'sos)    ;; StackOverflow Serach
;;   (package-install 'sos))

(autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t)
(global-set-key (kbd "C-;") 'ace-jump-mode)
(eval-after-load "ace-jump-mode" '(ace-jump-mode-enable-mark-sync))

(require 'ace-jump-buffer)
(global-set-key (kbd "C-,") 'ace-jump-buffer)

(require 'google-translate)
(require 'google-translate-default-ui)
;; (require 'google-translate-smooth-ui)

;; (load-theme 'tango t)
;; (load-theme 'greymatters)
(load-theme 'flatui t)

(require 'wgrep)
(require 'wgrep-ag)

(require 'mwim)
(global-set-key (kbd "C-a") 'mwim-beginning-of-code-or-line)
;; (global-set-key (kbd "C-e") 'mwim-end-of-code-or-line)

(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vsh\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.fsh\\'" . glsl-mode))

;;; email:

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

;; (setq mu4e-use-fancy-chars t)
(setq mu4e-maildir "~/Maildir")
(setq mu4e-drafts-folder "/[Gmail].Drafts")
(setq mu4e-sent-folder   "/[Gmail].Sent Mail")
(setq mu4e-trash-folder  "/[Gmail].Trash")

;; setup some handy shortcuts
;; you can quickly switch to your Inbox -- press ``ji''
;; then, when you want archive some messages, move them to
;; the 'archive' folder by pressing ``ma''.

(setq mu4e-maildir-shortcuts
      '( ("/INBOX"             . ?i)
         ("/[Gmail].Sent Mail" . ?s)
         ("/[Gmail].Trash"     . ?t)
         ("/[Gmail].All Mail"  . ?a)))

(setq mu4e-get-mail-command "offlineimap -q"
      mu4e-update-interval nil
      mu4e-view-show-images t
      mu4e-html2text-command "html2text -utf8"
      mu4e-headers-skip-duplicates t)

(setq user-mail-address "capak@inputwish.com"
      user-full-name  "Libor Čapák"
      mail-signature nil
      mail-signature-file nil
      message-signature nil
      mu4e-compose-signature nil
      mu4e-compose-signature-auto-include nil
      ;; message-signature-separator nil
      mu4e-sent-message-behaviour 'delete
      mu4e-hide-index-messages t
      mu4e-view-show-addresses t
      mu4e-date-format-long "%d.%m.%Y"
      mu4e-headers-date-format "%d.%m.%y"
      mu4e-confirm-quit nil)

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
(global-set-key (kbd "C-c m") 'mu4e)

;;; message view action
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

;; need this to convert some e-mails properly
;; (setq mu4e-html2text-command "html2text -utf8 -width 72")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("0ba649556dc51762e6794b92017f6f7406754ae3136eafef686d81c6da176cc5" default)))
 '(package-selected-packages
   (quote
    (flatui-theme wgrep-ag vc-darcs tup-mode sx swift-mode sublime-themes rust-mode repl-toggle python-mode pyde pixie-mode ninja-mode mwim lua-mode greymatters-theme google-translate glsl-mode expand-region exec-path-from-shell elpy define-word ag ace-jump-buffer))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
