;;; init.el --- emacs configuration file with more pleasant defaults
;; Copyright (C) 1999-2015 Jeff Rose
;         ___           ___           ___           ___           ___
;        /\__\         /\  \         /\  \         /\__\         /\__\
;       /:/ _/_       |::\  \       /::\  \       /:/  /        /:/ _/_
;      /:/ /\__\      |:|:\  \     /:/\:\  \     /:/  /        /:/ /\  \
;     /:/ /:/ _/_   __|:|\:\  \   /:/ /::\  \   /:/  /  ___   /:/ /::\  \
;    /:/_/:/ /\__\ /::::|_\:\__\ /:/_/:/\:\__\ /:/__/  /\__\ /:/_/:/\:\__\
;    \:\/:/ /:/  / \:\~~\  \/__/ \:\/:/  \/__/ \:\  \ /:/  / \:\/:/ /:/  /
;     \::/_/:/  /   \:\  \        \::/__/       \:\  /:/  /   \::/ /:/  /
;  ___ \:\/:/  /     \:\  \        \:\  \        \:\/:/  /     \/_/:/  /
; /\  \ \::/  /       \:\__\        \:\__\        \::/  /        /:/  /
; \/__/  \/__/         \/__/         \/__/         \/__/         \/__/

;; Author: Jeff Rose
;; URL: https://github.com/xjdr/emacs.d
;; Version: 0.2

(setq user-full-name "Jeff Rose")
(setq user-mail-address "jeff.rose12@gmail.com")

;; Makes life easier
(setenv "PATH" (concat "/usr/bin:" (getenv "PATH")))
(require 'cl)

;; SHHHH Splach screen & Stuff
(setq inhibit-splash-screen t
      initial-scratch-message nil)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)

;; -------~-------~--~------------------~------
;; SYNTAX
;; -------~-------~--~------------------~------

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; show matching parens
(show-paren-mode t)

;; hilight current line
;;(global-hl-line-mode 1)

;; ========== Place Backup Files in Specific Directory ==========

(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.emacs.d/auto-save-list"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
    version-control t)       ; use versioned backups

;; change the filename collisions in emacs
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))
(add-hook 'after-init-hook '(lambda ()
  (load "~/.emacs.d/site-lisp/emacs-tile.el")
  (load "~/.emacs.d/site-lisp/mode-line.el")
  (load "~/.emacs.d/site-lisp/google-c-style.el")
))

(require 'ido)
(ido-mode 'both)

(require 'ido-vertical-mode)
(ido-vertical-mode)

(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(add-hook 'dired-load-hook
	  (function (lambda () (load "dired-x"))))

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

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'junio t)
