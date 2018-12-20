;;; package --- init.el

;;; Commentary:

;;; Code:

;; Set PATH
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
			  "[ \t\n]*$"
			  ""
			  (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

(setq package--init-file-ensured t
      package-enable-at-startup nil
      package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/"))

      ;; security settings
      gnutls-verify-error t
      tls-checktrust gnutls-verify-error
      tls-program (list "gnutls-cli --x509cafile %t -p %p %h"
			;; compatibility fallbacks
			"gnutls-cli -p %p %h"
			"openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof")

       ;; Don't track MELPA, we'll use package.el for that
      quelpa-checkout-melpa-p nil
      quelpa-update-melpa-p nil
      quelpa-melpa-recipe-stores nil
      quelpa-self-upgrade-p nil
      byte-compile-dynamic nil
      byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

(condition-case nil
    (require 'use-package)
  (file-error
   (package-initialize)
   (package-refresh-contents)
   (package-install 'use-package)
   (require 'use-package)))

;; UI
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(fringe-mode '(0 . 0))
(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-splash-screen t ring-bell-function 'ignore)
(set-face-attribute 'default nil :height 125 :font "Hack")

;; Setup
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))     ; pretty
(prefer-coding-system        'utf-8)   ; pretty
(set-terminal-coding-system  'utf-8)   ; pretty
(set-keyboard-coding-system  'utf-8)   ; pretty
(set-selection-coding-system 'utf-8)   ; perdy
(setq locale-coding-system   'utf-8)   ; please
(setq-default buffer-file-coding-system 'utf-8) ; with sugar on top

(setq-default
 ad-redefinition-action 'accept   ; silence advised function warnings
 apropos-do-all t                 ; make `apropos' more useful
 compilation-always-kill t        ; kill compilation process before starting another
 compilation-ask-about-save nil   ; save all buffers on `compile'
 compilation-scroll-output t
 confirm-nonexistent-file-or-buffer t
 enable-recursive-minibuffers nil
 idle-update-delay 2              ; update ui less often
 ;; keep the point out of the minibuffer
 minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)
 ;; History & backup settings (save nothing, that's what git is for)
 auto-save-default nil
 create-lockfiles nil
 history-length 500
 make-backup-files nil
 ;; files
 abbrev-file-name             (concat  user-emacs-directory "local/abbrev.el")
 auto-save-list-file-name     (concat  user-emacs-directory "cache/autosave")
 backup-directory-alist       (list (cons "." (concat  user-emacs-directory "cache/backup/")))
 pcache-directory             (concat  user-emacs-directory "cache/pcache/")
 mc/list-file                 (concat  user-emacs-directory "etc/mc-lists.el")
 server-auth-dir              (concat  user-emacs-directory "cache/server/")
 tramp-auto-save-directory    (concat  user-emacs-directory "cache/tramp-auto-save/")
 tramp-backup-directory-alist backup-directory-alist
 tramp-persistency-file-name  (concat  user-emacs-directory "cache/tramp-persistency.el"))

;; move custom defs out of init.el
(setq custom-file (concat  user-emacs-directory "etc/custom.el"))
(load custom-file t t)

;; Whitespace
(require 'whitespace)
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Misc
(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode)
  :commands (hl-todo-previous
	     hl-todo-next
	     hl-todo-occur))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (show-paren-mode))

(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-light t)
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :background "#f9f2d9")))

(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-c g") 'magit-status))

(use-package moody
  :ensure t
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode)
  :commands (hl-todo-previous
	     hl-todo-next
	     hl-todo-occur))

(use-package counsel
  :ensure t
  :commands (counsel-load-theme
	     counsel-bookmark)
  :bind* (("C-c i" . counsel-imenu)
	  ("C-x b" . ivy-switch-buffer)
	  ("C-x C-f" . counsel-find-file)
	  ("C-c C-/" . counsel-rg)
	  ("M-y" . counsel-yank-pop)
	  ("M-x" . counsel-M-x))
  :config
  (setq counsel-find-file-ignore-regexp
	(concat "\\(?:\\`[#.]\\)\\|\\(?:[#~]\\'\\)"
		"\\|\\.x\\'\\|\\.d\\'\\|\\.o\\'"
		"\\|\\.aux\\'"))

  (setq counsel-locate-cmd 'counsel-locate-cmd-mdfind)
  (setq counsel-find-file-at-point t))

(use-package swiper
  :ensure t
  :bind* ("M-s" . swiper))

(use-package ivy
  :ensure t
  :diminish ""
  :after (counsel swiper)
  :custom
  (ivy-display-style 'fancy)
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config
  (ivy-mode 1)
  (setq ivy-display-function nil))

;; Eshell
;; Colorize strings:
(defmacro with-face (str &rest properties)
  "Return STR with the given face PROPERTIES, suitable for `concat'."
  `(propertize ,str 'face (list ,@properties)))

(require 'eshell)
(setq eshell-visual-commands '("bash" "htop"))
(setq eshell-visual-subcommands
      '(("git" "log" "l" "ll" "diff" "show")))
(setq eshell-prompt-regexp "^> ")
(setq eshell-highlight-prompt nil)
(setq eshell-prompt-function
      (lambda ()
	(let ((red       "#dc322f")
	      (purple    "#FFCCFF")
	      (magenta   "#d33682")
	      (base      "#839496"))
	  (concat
	   (let ((status eshell-last-command-status))
	     (when (not (= status 0))
	       (with-face (concat "[" (number-to-string status) "] ") :foreground "#dc322f")))
	   (with-face "┌─@" :foreground (if (= (user-uid) 0) "#dc322f" "#268bd2"))
	   (with-face (system-name) :foreground "#b58900") " "
	   (with-face (replace-regexp-in-string (concat "\\`" (getenv "HOME")) "~" (eshell/pwd))
		      :foreground "#268bd2")
	   ;; TODO: Display more Git info.
	   (let ((head (shell-command-to-string "git describe --contains --all HEAD")))
	     (unless (string-match "fatal:" head)
	       (concat "[" (with-face (replace-regexp-in-string "\n\\'" "" head)
		      :foreground "#d33682") "] "))) "\n"
	   (with-face ">":foreground "#268bd2") " "))))

;; Toggle eshell function
(fset 'eshell-on
      "\C-x1\C-x3\C-xo\M-xeshell")

(fset 'eshell-off
      "\C-x0")

(defun toggle-eshell ()
  (interactive)
  (if (string= "eshell-mode" (eval 'major-mode))
      (execute-kbd-macro (symbol-function 'eshell-off))
    (execute-kbd-macro (symbol-function 'eshell-on))))


;; Custom Key Bindings
(local-unset-key (kbd "C-c C-c"))

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-d") 'dired-jump)
(global-set-key (kbd "C-x t") 'toggle-eshell)
(global-set-key (kbd "s-<return>") 'toggle-frame-fullscreen)
(global-set-key (kbd "s-/") 'comment-or-uncomment-region)
(global-set-key (kbd "s-f") 'grep-find)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-c C-c") 'compile)
(global-set-key (kbd "s-t") 'neotree-toggle)
