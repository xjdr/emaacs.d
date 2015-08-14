;;; init.el --- emacs configuration file with more pleasant defaults
;; Copyright (C) 1999-2014 Jeff Rose
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
;; URL: https://github.com/jeffrose12/emacs
;; Version: 0.01
;; Keywords: emacs, dotfile, config

(setq user-full-name "Jeff Rose")
(setq user-mail-address "jefrose@paypal.com")

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

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; show matching parens
(show-paren-mode t)

;; hilight current line
(global-hl-line-mode 1)

;; ========== Place Backup Files in Specific Directory ==========

;; Enable backup files.
(setq make-backup-files t)

;; Save all backup file in this directory.
(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))

;; change the filename collisions in emacs
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))
(add-hook 'after-init-hook '(lambda ()
  (load "~/.emacs.d/site-lisp/emacs-tile.el")
  (load "~/.emacs.d/site-lisp/mode-line.el")
))

(require 'ido)
(ido-mode 'both)

(require 'ido-vertical-mode)
(ido-vertical-mode)

(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(add-hook 'dired-load-hook
            (function (lambda () (load "dired-x"))))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'junio t)

(require 'whitespace)
 (setq whitespace-style '(face empty tabs lines-tail trailing))
 (global-whitespace-mode t)
  (custom-set-faces
   '(my-tab-face            ((((class color)) (:background "grey10"))) t)
   '(my-trailing-space-face ((((class color)) (:background "gray10"))) t)
   '(my-long-line-face ((((class color)) (:background "gray10"))) t))
 (add-hook 'font-lock-mode-hook
            (function
             (lambda ()
               (setq font-lock-keywords
                     (append font-lock-keywords
                             '(("\t+" (0 'my-tab-face t))
                               ("^.\\{81,\\}$" (0 'my-long-line-face t))
                               ("[ \t]+$"      (0 'my-trailing-space-face t))))))))
