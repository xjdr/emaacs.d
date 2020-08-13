(setq user-full-name "Jeff Rose")
(setq user-mail-address "")

(setenv "PATH" (concat "/usr/local/bin:/usr/bin:/bin:/usr/local/games:/usr/games:/opt/go/bin" (getenv "PATH")))
(setq exec-path (append exec-path '("/usr/local/bin")))

(defconst emacs-start-time (current-time))
(setq message-log-max 16384)

;; Get rid of some superfluous cruft
(setq ring-bell-function 'ignore)

(defun emacs-d (filename)
  "Expand FILENAME relative to `user-emacs-directory'."
  (expand-file-name filename user-emacs-directory))

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

(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)
(defalias 'yes-or-no-p 'y-or-n-p)
(show-paren-mode t)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(global-auto-revert-mode 1)
(fringe-mode '(1 . 0))

(if window-system
    (progn
      (load-theme 'wombat t)
      (load-theme 'deeper-blue t)))

;;;; Ido
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


(set-face-attribute 'default nil :height 105) ; Laptop
;;(set-face-attribute 'default nil :height 105) ; Monitor

(defconst my/eclipse-jdt-home "/home/xjdr/src/eclipse/eclipse.jdt.ls/org.eclipse.jdt.ls.product/target/repository
")

(defun my/eclipse-jdt-contact (interactive)
  (let ((cp (getenv "CLASSPATH")))
    (setenv "CLASSPATH" (concat cp ":" my/eclipse-jdt-home))
    (unwind-protect
        (eglot--eclipse-jdt-contact nil)
      (setenv "CLASSPATH" cp))))

(setcdr (assq 'java-mode eglot-server-programs) #'my/eclipse-jdt-contact)

(use-package hl-todo
  :ensure t
  :defer 2)

 (use-package moody
   :ensure t
   :defer 2
   :config
   (setq x-underline-at-descent-line t)
   (moody-replace-mode-line-buffer-identification)
   (moody-replace-vc-mode)
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line           nil :overline   line)
    (set-face-attribute 'mode-line-inactive  nil :overline   line)
    (set-face-attribute 'mode-line-inactive  nil :underline  line)
    (set-face-attribute 'mode-line           nil :box        nil)
    (set-face-attribute 'mode-line-inactive  nil :box        nil)
    (set-face-attribute 'mode-line-buffer-id nil :foreground "white")
    (set-face-attribute 'mode-line           nil :foreground "white")
    (set-face-attribute 'mode-line           nil :background "#778899")
    (set-face-attribute 'mode-line-inactive  nil :background "#2F4F4F"))
  )

(use-package projectile
  :ensure t
  :defer 2)

(use-package magit
  :ensure t
  :defer 2
  :bind
  ("C-x g" . magit-status))

(use-package eglot
  :ensure t
  :defer 2)

(use-package company
  :ensure t
  :defer 2)

(use-package company-quickhelp
  :ensure t
  :defer 2)

(use-package kotlin-mode
  :ensure t
  :defer 2)

(use-package go-mode
  :ensure t
  :defer 2)

(use-package markdown-mode
  :ensure t
  :defer 2
)

(use-package bazel-mode
  :ensure t
  :defer 2
)

(use-package protobuf-mode
  :ensure t
  :defer 2
)

(use-package cmake-mode
  :ensure t
  :defer 2
)

(use-package swiper
  :ensure t
  :defer 2
  :bind
  ("C-s" . swiper-isearch)
  ("C-r" . swiper-isearch-backward))

;; (use-package counsel
;;   :ensure t
;;   :defer 2
;;   :config
;;   (setq counsel-find-file-ignore-regexp "\\(?:\\`\\.\\|elc\\|pyc$\\|class$\\)")
;;   (setq counsel-rg-base-command
;;         "rg --sort path -M 120 --no-heading --line-number --color never %s")
;;   (setq counsel-git-grep-cmd-default
;;       (concat "git --no-pager grep --full-name -n --no-color -i -e '%s' -- './*' "
;;               (mapconcat (lambda (x) (format "':!*.%s'" x))
;;                          '("htm" "so" "a" "TTC" "NDS" "png" "md5") " ")))
;; )

;; (use-package ivy
;;   :ensure t
;;   :config
;;   (ivy-mode 1)
;;   (setq ivy-display-style 'fancy)
;;   (setq ivy-use-virtual-buffers t)
;;   )

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-fIlter-hook 'colorize-compilation-buffer)

;; Eshell
(defmacro with-face (str &rest properties)
  "Return STR with the given face PROPERTIES, suitable for `concat'."
  `(propertize ,str 'face (list ,@properties)))

(require 'eshell)
(setq eshell-visual-commands '("zsh" "bash" "htop"))
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

(global-set-key (kbd "C-x t") 'toggle-eshell)
(global-set-key (kbd "C-c f") 'counsel-rg)
(global-set-key (kbd "C-.") 'find-file-at-point)
(global-set-key (kbd "C-c c") 'projectile-compile-project)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(bazel-mode protobuf-mode cmake-mode counsel swiper markdown-mode go-mode kotlin-mode company-quickhelp company eglot magit projectile moody hl-todo use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
