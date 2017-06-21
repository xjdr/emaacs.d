;;  init.el
;;  xjdr - here be dragons and whatnot

;;; load custom settings
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

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

;; shh
(setq ring-bell-function 'ignore)

;; home sweet home
(defun emacs-d (filename)
  "Expand FILENAME relative to `user-emacs-directory'."
  (expand-file-name filename user-emacs-directory))

;; Annoyances
(setq inhibit-splash-screen t
      ring-bell-function 'ignore)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(set-fringe-mode 0)
(unless window-system
  (menu-bar-mode 0))
(fset 'yes-or-no-p 'y-or-n-p)

;; NO SAVES
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files

;; Encodings
(set-terminal-coding-system 'utf-8)     ; always use UTF-8
(set-keyboard-coding-system 'utf-8)     ; it is the future
(prefer-coding-system 'utf-8)

;; Cocoa stuffs
(defun xjdr/configure-cocoa ()
  ;; open up maximized-ish
  (let ((px (display-pixel-width))
	(py (display-pixel-height))
	(fx (frame-char-width))
	(fy (frame-char-height))
	tx ty)
    (setq tx (- (/ px fx) 7))
    (setq ty (- (/ py fy) 4))
    (setq initial-frame-alist '((top . 2) (left . 2)))
    (add-to-list 'default-frame-alist (cons 'width tx))
    (add-to-list 'default-frame-alist (cons 'height ty)))

  ;; don't scroll like a maniac
  (setq mouse-wheel-scroll-amount '(1))
  (setq mouse-wheel-progressive-speed nil))
(if (memq window-system '(mac ns)) (xjdr/configure-cocoa))

;; Extra Stuffs
(electric-pair-mode t)                  ; automatically pair quotes and such
(delete-selection-mode t)               ; delete selections when yanking etc
(windmove-default-keybindings 'super)   ; bind windmove to s-{arrows}

;;; bootstrap `use-package'
(require 'package)

(setq package-archives
      (append package-archives
	      (add-to-list 'package-archives
			   '("melpa" . "https://melpa.org/packages/"))))

(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Ido
(ido-mode 1)
(ido-everywhere 1)
(setq ido-use-virtual-buffers t
      recentf-save-file (emacs-d "var/recentf")
      save-place-file (emacs-d "var/saved-places")
      ido-save-directory-list-file (emacs-d "var/ido-last.el"))

;; Display completions vertically
(setq ido-decorations (quote ("\n> " "" "\n  " "\n  ..." "[" "]"
			      " [No Match]" " [Matched]" " [Not Readable]"
			      " [Too Big]" " [Confirm]")))

(defun ido-disable-line-truncation ()
  (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)

(defun ido-define-keys ()
  "C-(n|p) is more intuitive in vertical layout."
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
(add-hook 'ido-setup-hook 'ido-define-keys)

;; ibuffer
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Keyboard
(when (string= system-type "darwin")
  (setq mac-option-modifier 'meta
	mac-command-modifier 'super
	delete-by-moving-to-trash t
	trash-directory (expand-file-name ".Trash" (getenv "HOME"))))

;; Whitespace
(require 'whitespace)
(add-hook 'before-save-hook 'whitespace-cleanup)

;;;; *scratch* buffer
(setq initial-scratch-message nil)
(setq initial-major-mode 'org-mode)

;; Never kill, just bury
(defun dont-kill-but-bury-scratch ()
  "Don't kill but burry *scratch* buffer."
  (if (equal (buffer-name (current-buffer)) "*scratch*")
      (progn (bury-buffer) nil)
    t))
(add-hook 'kill-buffer-query-functions 'dont-kill-but-bury-scratch)

;; Misc
(show-paren-mode)

;;;; Tramp is good
(require 'tramp)
(setq tramp-default-method "ssh")
(setq tramp-persistency-file-name (emacs-d "var/tramp-history.el"))

;; eshell
(setq eshell-prompt-function (lambda nil
			       (concat
				(propertize (eshell/pwd) 'face `(:foreground "#A074C4"))
				(propertize " $ " 'face `(:foreground "black")))))
(setq eshell-highlight-prompt nil)

(fset 'eshell-on
      "\C-x3\C-xo\M-xeshell")

(fset 'eshell-off
      "\C-x0")

(defun toggle-eshell ()
  (interactive)
  (if (string= "eshell-mode" (eval 'major-mode))
      (execute-kbd-macro (symbol-function 'eshell-off))
    (execute-kbd-macro (symbol-function 'eshell-on))))

(global-set-key (kbd "C-x t") 'toggle-eshell)


;; Compiler
;; colorize the output of the compilation mode.
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))

  ;; mocha seems to output some non-standard control characters that
  ;; aren't recognized by ansi-color-apply-on-region, so we'll
  ;; manually convert these into the newlines they should be.
  (goto-char (point-min))
  (while (re-search-forward "�\\[2K�\\[0G" nil t)
    (progn
      (replace-match "
")))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)


;; Packages
(use-package ag
  :ensure t
  :defer t
  :config
  (setq ag-highlight-search t))

(use-package editorconfig
  :ensure t)

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package git-commit
  :ensure t
  :config
  (add-hook 'git-commit-mode-hook 'flyspell-mode))

(use-package js2-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(use-package json-mode
  :ensure t)

(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-c g") 'magit-status))

(use-package markdown-mode
  :ensure t)

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (setq projectile-enable-caching t
	projectile-cache-file (emacs-d "var/projectile.cache")
	projectile-known-projects-file (emacs-d "var/projectile-bookmarks.eld")))

(use-package protobuf-mode
  :ensure t)

(use-package web-mode
  :ensure t
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-enable-auto-pairing t)
  ;; add navigation to Soy templates
  (add-to-list 'web-mode-imenu-regexp-list
	       '("^{\\(template\\)[ ]+\\([^ ]+\\).*$" 1 2 " "))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.soy\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

(use-package yaml-mode
  :ensure t)

;; Load modules
(load (emacs-d "go") 'missing-ok)
(load (emacs-d "c++") 'missing-ok)
(load (emacs-d "java") 'missing-ok)
(load (emacs-d "python") 'missing-ok)
(load (emacs-d "clojure") 'missing-ok)
(load (emacs-d "hipster-theme") 'missing-ok)

;; Custom Key Bindings down here so they aren't overwritten by somethingc
(global-set-key (kbd "s-<return>") 'toggle-frame-fullscreen)
(global-set-key (kbd "s-/") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-c C-c") 'compile)
(global-set-key (kbd "C-c C-t") (lambda () (interactive) (compile "make -k test")))
