;;; init.el --- emacs configuration file with more pleasant defaults
;; Copyright (C) 1999-2015 Jeff Rose

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

(setq backup-inhibited t)

(setq auto-save-default nil)


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

;; java
(add-hook 'java-mode-hook
          (lambda ()
            "Treat Java 1.5 @-style annotations as comments."
            (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
            (modify-syntax-entry ?@ "< b" java-mode-syntax-table)
            (setq c-basic-offset 2
                  tab-width 2
                  indent-tabs-mode t)))

;; scala
(add-to-list 'load-path "~/.emacs.d/vendor/scala-mode2/")
(require 'scala-mode2)

;; clojure
(load "~/.emacs.d/site-lisp/clojure-mode.el")
(load "~/.emacs.d/site-lisp/clojure-mode-extra-font-locking.el")
(require 'clojure-mode)

;; theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'junio t)

