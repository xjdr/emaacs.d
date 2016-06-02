;; Lets set up some emacs
(blink-cursor-mode 0)
(setq initial-scratch-message nil)
(setq inhibit-startup-message t)

;; Why
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(set-fringe-mode 0)
(setq ring-bell-function 'ignore)

(setq tramp-default-method "ssh")
(defalias 'yes-or-no-p 'y-or-n-p)

;; Keep ya backups out my dirs
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; Use spaces, not tabs for indentation:
(setq-default indent-tabs-mode nil)

;; Show trailing whitespaces:
(require 'whitespace)
(setq-default show-trailing-whitespace t)
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Highlight matching parens:
(show-paren-mode t)

;; Lets get some packages
(load "package")
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("elpy" . "https://jorgenschaefer.github.io/packages/"))

;; uncomment the next line if the package cache is corrupt
;;(package-refresh-contents)
(package-initialize)

; list the packages you want
(setq package-list '(smex
                     editorconfig
                     google-c-style
                     ido-vertical-mode))

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; ido
(require 'ido)
(ido-mode 1)

;; ido-vertical
(require 'ido-vertical-mode)
(ido-vertical-mode)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)
(setq ido-use-faces t)
(set-face-attribute 'ido-vertical-first-match-face nil
                    :background nil
                    :foreground "green")
(set-face-attribute 'ido-vertical-only-match-face nil
                    :background nil
                    :foreground nil)
(set-face-attribute 'ido-vertical-match-face nil
                    :foreground nil)

;; smex
(require 'smex)
(global-set-key (kbd "M-x") 'smex)
(setq smex-save-file "~/.emacs.d/smex.save") ;; keep my ~/ clean

(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(require 'dired-x)
(add-hook 'dired-load-hook
          (function (lambda () (load "dired-x"))))

;; Ansi Term
(defvar my-term-shell "/usr/local/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

;; Custom key bindings
(global-set-key (kbd "s-<return>") 'toggle-frame-fullscreen)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "s-/") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-c C-k") 'compile)
(global-set-key (kbd "C-x t") 'ansi-term)
(global-set-key (kbd "C-x c e") 'flymake-display-err-menu-for-current-line)
(global-set-key (kbd "C-x c n") 'flymake-goto-next-error)
(global-set-key (kbd "C-x c p") 'flymake-goto-prev-error)

;; window navigation keys
(global-set-key "\C-ch" 'windmove-left)
(global-set-key "\C-c\C-h" 'windmove-left)
(global-set-key "\C-cj" 'windmove-down)
(global-set-key "\C-c\C-j" 'windmove-down)
(global-set-key "\C-ck" 'windmove-up)
(global-set-key "\C-c\C-k" 'windmove-up)
(global-set-key "\C-cl" 'windmove-right)
(global-set-key "\C-c\C-l" 'windmove-right)

;; Org
(setq ispell-program-name "/usr/local/bin/aspell")
(setq ispell-dictionary "english")

(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-buffer)

;; C++
(defun my-c++-mode-hook ()
  (google-set-c-style)
  (google-make-newline-indent))

(add-hook 'c++-mode-hook
           (lambda ()
             (flymake-mode)
             (my-c++-mode-hook)
            (editorconfig-mode)))

;; Java
(add-hook 'java-mode-hook
          (lambda ()
            (flymake-mode)
            (editorconfig-mode)))

;; Python
(setq python-shell-interpreter "/usr/local/bin/ipython"
     python-shell-interpreter-args "-i")
(add-hook 'python-mode-hook
                    (lambda ()
            (flymake-mode)
            (editorconfig-mode)))

;; Theme?
(custom-set-faces
 '(default ((t (:inherit nil :stipple nil :background "Black" :foreground "White"))))   ;  :inverse-video nil :box nil :strike-t*hrough nil :overline nil :underline nil :slant normal :weight normal :width normal :height 105))))
 '(highlight ((((class color) (min-colors 88) (background dark)) (:background "#111111"))))
 '(region ((nil (:background "#255255"))))
 '(hl-line ((nil (:background "#222222"))))
 '(font-lock-warning-face ((nil (:foreground "#ff6666"))))
 '(show-paren-match ((nil (:background "#1793d0"))))
 '(show-paren-mismatch((((class color)) (:background "red")))))

(set-face-attribute 'mode-line nil
   :foreground "#1793d0"
   :background "#111111"
   :box '(:line-width 6 :color "#111111" :style nil))
