;;; init.el --- emacs configuration file with more pleasant defaults
;; Copyright (C) 1999-2015 Jeff Rose

;; Author: Jeff Rose
;; URL: https://github.com/xjdr/emacs.d
;; Version: 0.3

(setq user-full-name "Jeff Rose")
(setq user-mail-address "jeff.rose12@gmail.com")

; list the packages you want
(setq package-list '(magit yaml-mode))

(when (>= emacs-major-version 24)
																				; list the repositories containing them
	(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
													("gnu" . "http://elpa.gnu.org/packages/")
													("marmalade" . "http://marmalade-repo.org/packages/")))
																				; activate all the packages (in particular autoloads)
	(package-initialize)
																				; fetch the list of packages available
	(unless package-archive-contents
		(package-refresh-contents))
																				; install the missing packages
	(dolist (package package-list)
		(unless (package-installed-p package)
			(package-install package))))

;; Makes life easier
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(require 'cl)

;; NO BACKUPS
;disable backup
(setq backup-inhibited t)
;disable auto save
(setq auto-save-default nil)

;; SHHHH Splach screen & Stuff
(setq inhibit-splash-screen t
			initial-scratch-message nil
			visible-bell 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 0)
(global-set-key "\M-\r" 'toggle-frame-fullscreen)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Load site-lisp
(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))
(add-hook 'after-init-hook '(lambda ()
															(load "~/.emacs.d/site-lisp/emacs-tile.el")
															(load "~/.emacs.d/site-lisp/mode-line.el")
															(load "~/.emacs.d/site-lisp/google-c-style.el")
))

;; Syntax
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; make tab key always call a indent command.
(setq-default tab-always-indent t)

;; Font lock
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

(require 'better-defaults)

(require 'ido)
(ido-mode 'both)

(require 'ido-vertical-mode)
(ido-vertical-mode)

(require 'smex)
(global-set-key (kbd "M-x") 'smex)

(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; c++ 
(defun my-c++-mode-hook ()
	(google-set-c-style)
	(google-make-newline-indent))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)

;; python
(add-hook 'python-mode-hook
					(lambda () 
						(setq indent-tabs-mode t)
						(setq python-indent 2)
						(setq tab-width 2))
						(tabify (point-min) (point-max)))

;; java
(add-hook 'java-mode-hook
					(lambda ()
						"Treat Java 1.5 @-style annotations as comments."
						(setq c-comment-start-regexp "(@|/(/|[*][*]?))")
						(modify-syntax-entry ?@ "< b" java-mode-syntax-table)
							(google-set-c-style)
							(google-make-newline-indent)))

;; markdown
;;(load "~/.emacs.d/site-lisp/markdown-mode.el")
(require 'markdown-mode)
(autoload 'markdown-mode "markdown-mode"
	 "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;; Custom Key Binding
(global-set-key (kbd "M-j")
						(lambda ()
									(interactive)
									(join-line -1)))

(global-set-key (kbd "s-/")
								(lambda ()
									(interactive)
									(comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(global-set-key (kbd "C-o")
								(lambda ()
									(interactive)
									(previous-line)
									(end-of-line)
									(newline)))

(global-set-key (kbd "M-s") 'magit-status)

;; theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/solarized")
(customize-set-variable 'frame-background-mode 'dark)
(load-theme 'solarized t)
