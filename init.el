;;;Lets actually try to get better at this

(setq user-full-name "Jeff Rose")
(setq user-mail-address "jeff.rose12@gmail.com")

;; Set up the env
(setenv "PATH" (concat "/usr/texbin:/usr/local/bin:/usr/bin:/bin" (getenv "PATH")))
(setenv "GOPATH" (concat (getenv "HOME") "/src/go"))
(add-to-list 'exec-path (concat (getenv "GOPATH") "/bin"))
(require 'cl)

;; Finally, lets set up some emacs
(setq inhibit-splash-screen t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 0)
(setq ring-bell-function 'ignore)

;; Lets get some packages
(load "package")
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/") t)

(setq tramp-default-method "ssh")
(defalias 'yes-or-no-p 'y-or-n-p)

(setq backup-inhibited t)
(setq auto-save-default nil)
(setq make-backup-files nil)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(show-paren-mode t)

(setq whitespace-style '(face trailing tabs tab-mark))
(global-whitespace-mode)

;; Configure some default emacs packages
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'ido)
(ido-mode 'both)

;(require 'ido-vertical-mode)
;(ido-vertical-mode)

;(require 'smex)
;(global-set-key (kbd "M-x") 'smex)

(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(add-hook 'dired-load-hook
          (function (lambda () (load "dired-x"))))

;; Custom key bindings
;(global-set-key (kbd "M-RET") 'toggle-frame-fullscreen)
(global-set-key (kbd "s-<return>") 'toggle-frame-fullscreen)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "s-/") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-c C-k") 'compile)
;(global-set-key (kbd "C-x g") 'magit-status)

;; Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'zenburn t)

;; Modeline
;(set-face-attribute 'mode-line nil
;    :foreground "gray60" :background "gray20"
;    :inverse-video nil
;    :box '(:line-width 6)); :color "gray20" :style nil))

;; Set up some Language Specific Stuffs

;; Org
(setq ispell-program-name "/usr/local/bin/aspell") ; could be ispell as well, depending on your preferences
(setq ispell-dictionary "english") ; this can obviously be set to any language your spell-checking program supports

(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-buffer)
