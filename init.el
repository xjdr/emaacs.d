;;; package --- init.el

;;; Commentary:

;;; Code:

;; Set PATH
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -c 'echo $PATH'"))))
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
(set-face-attribute 'default nil :height 125 :font "Hack")  ;; TODO Add Darwin | linux config (probably don't have hack on linux)

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
 minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)
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

(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox t)
  (let ((line (face-attribute 'mode-line :underline)))))

;; (use-package solarized-theme
;;   :ensure t
;;   :config
;;   (load-theme 'solarized-light t)
;;   (let ((line (face-attribute 'mode-line :underline)))
;;     (set-face-attribute 'mode-line          nil :overline   line)
;;     (set-face-attribute 'mode-line-inactive nil :overline   line)
;;     (set-face-attribute 'mode-line-inactive nil :underline  line)
;;     (set-face-attribute 'mode-line          nil :box        nil)
;;     (set-face-attribute 'mode-line-inactive nil :box        nil)
;;     (set-face-attribute 'mode-line-inactive nil :background "#f9f2d9")))

(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-c g") 'magit-status))

;(use-package moody
;  :ensure t
;  :config
;  (setq x-underline-at-descent-line t)
;  (setq moody-slant-function #'moody-slant-apple-rgb)
;  (moody-replace-mode-line-buffer-identification)
;  (moody-replace-vc-mode))

(use-package smex
  :ensure t)

(use-package counsel
  :ensure t
  :commands (counsel-load-theme
             counsel-bookmark)
  :bind* (("C-c i" . counsel-imenu)
          ("C-x b" . ivy-switch-buffer)
          ("C-c /" . counsel-rg)
          ("C-x C-f" . counsel-find-file)
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
  (setq ivy-display-function nil)
  (set-face-attribute 'ivy-current-match nil :background "#3c3836")) ;; for gruvbox theme only

(use-package ibuffer
  :ensure t)

;; Flycheck
(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flycheck-rust
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-disabled-checkers '(javascript-jshint javascript-jscs))))

(add-to-list 'flycheck-checkers 'python-pylint 'python-mypy)
(flycheck-add-mode 'javascript-eslint 'js-mode)

(defun counsel-flycheck ()
  (interactive)
  (if (not (bound-and-true-p flycheck-mode))
      (message "Flycheck mode is not available or enabled")
    (ivy-read "Error: "
              (let ((source-buffer (current-buffer)))
                (with-current-buffer (or (get-buffer flycheck-error-list-buffer)
                                         (progn
                                           (with-current-buffer
                                               (get-buffer-create flycheck-error-list-buffer)
                                             (flycheck-error-list-mode)
                                             (current-buffer))))
                  (flycheck-error-list-set-source source-buffer)
                  (flycheck-error-list-reset-filter)
                  (revert-buffer t t t)
                  (split-string (buffer-string) "\n" t " *")))
              :action (lambda (s &rest _)
                        (-when-let* ( (error (get-text-property 0 'tabulated-list-id s))
                                      (pos (flycheck-error-pos error)) )
                          (goto-char (flycheck-error-pos error))))
              :history 'counsel-flycheck-history)))

;; Languages

;; Rust
(use-package rust-mode
  :ensure t
  :config
  (setq rust-format-on-save t)
  (add-hook 'rust-mode-hook
            (lambda ()
              (setq rust-format-on-save t)
              (setq-local company-backends (list 'company-lsp))
              (if (eq system-type 'darwin)
                  (setq flycheck-rust-clippy-executable "/Users/xjdr/.cargo/bin/cargo-clippy"))
              (if (eq system-type 'gnu/linux)
                  (setq flycheck-rust-clippy-executable "/home/xjdr/.cargo/bin/cargo-clippy"))))
  (add-hook 'rust-mode-hook 'flycheck-mode)
  (add-hook 'rust-mode-hook 'company-mode))

(use-package toml-mode
  :ensure t)

(use-package racer
  :requires rust-mode
  :init (setq racer-rust-src-path
              (concat (string-trim
                       (shell-command-to-string "rustc --print sysroot"))
                      "/lib/rustlib/src/rust/src"))
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode))

;; C++
(use-package google-c-style
  :ensure t)

(add-hook 'c++-mode-hook
          (lambda ()
            (google-set-c-style)
            (google-make-newline-indent)))
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c++-mode-hook 'company-mode)
(add-hook 'c++-mode-hook 'lsp)

;; Python
(if (eq system-type 'darwin)
    (setq  python-shell-interpreter "/usr/local/bin/ipython"))
(if (eq system-type 'gnu/linux)
    (setq  python-shell-interpreter "/usr/bin/ipython3"))
(setq
 python-shell-interpreter-args "-i --simple-prompt --colors=Linux --profile=default"
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
 "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
 "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
 "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"
ansi-color-for-comint-mode t)

(add-hook 'python-mode-hook
    (lambda ()
      (setq flycheck-python-pylint-executable (concat (vc-call-backend 'Git 'root default-directory) "/venv/bin/pylint"))
      (setq flycheck-pylintrc (concat (vc-call-backend 'Git 'root default-directory) ".pylintrc"))
      (setq tab-width 2)
      (setq python-indent-offset 2)))

;; Javascript
(add-hook 'js-mode-hook
            (lambda ()
              (setq flycheck-javascript-eslint-executable (concat (vc-call-backend 'Git 'root default-directory) "node_modules/eslint/bin/eslint.js"))))
  (add-hook 'js-mode-hook
            (lambda ()
              (setq tab-width 2)
              (setq js-indent-level 2)))

;; LSP
(use-package editorconfig
  :ensure
  :config (editorconfig-mode))

(use-package projectile
  :ensure t)

(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook (prog-mode . lsp)
  :config (require 'lsp-clients)
  :init
  (if (eq system-type 'darwin)
      (setq lsp-clients-clangd-executable "/usr/local/opt/llvm/bin/clangd"))
  (if (eq system-type 'gnu/linux)
      (setq lsp-clients-clangd-executable "/usr/bin/clangd-8")))

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (set-face-attribute 'lsp-ui-doc-background nil :background "#3c3836"))

(use-package hydra
  :ensure t)

(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode))

(use-package company
  :ensure t)

(use-package company-lsp
  :after  company
  :ensure t
  :config
  (setq company-lsp-cache-candidates t
        company-lsp-async t))

;; Java
(use-package lsp-java
  :ensure t
  :config
  (setq lsp-java-inhibit-message t)
  (setq lsp-ui-sideline-update-mode 'point)
  (add-hook 'java-mode-hook
            (lambda ()
              (setq-local company-backends (list 'company-lsp))))
  (add-hook 'java-mode-hook 'lsp)
  (add-hook 'java-mode-hook 'flycheck-mode)
  (add-hook 'java-mode-hook 'company-mode))

;; Config
(use-package yaml-mode
  :ensure t)

(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :config
  (setq markdown-fontify-code-blocks-natively t))

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

;;; TAB behavior
(setq tab-always-indent 'complete)
(setq-default indent-tabs-mode nil)   ; never use tabs to indent.

;; Whitespace
(setq whitespace-style '(face tabs tab-mark trailing))
(custom-set-faces
 '(whitespace-tab ((t (:foreground "#636363")))))
(setq whitespace-display-mappings
      '((tab-mark 9 [124 9] [92 9]))) ; 124 is the ascii ID for '\|'
(global-whitespace-mode) ; Enable whitespace mode everywhere
                                        ; END TABS CONFIG

;; ediff
(setq ediff-diff-options "-w"
      ediff-split-window-function #'split-window-horizontally
      ediff-window-setup-function #'ediff-setup-windows-plain)

;; Find with rg
(require 'grep)
(grep-apply-setting 'grep-find-command
                    "rg -i -M 120 --no-heading --line-number --color never ") ;; TODO Make sure ripgrep is installed

;; Find with ag
;; (grep-apply-setting 'grep-find-command
;;                     "ag --vimgrep ")


;; Custom Key Bindings
(local-unset-key (kbd "C-x f"))
(local-unset-key (kbd "C-c C-c"))

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-d") 'dired-jump)
(global-set-key (kbd "C-x t") 'toggle-eshell)
(global-set-key (kbd "s-<return>") 'toggle-frame-fullscreen)
(global-set-key (kbd "s-/") 'comment-or-uncomment-region)
(global-set-key (kbd "s-f") 'grep-find)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-c C-c") 'compile)
(global-set-key (kbd "C-x f") 'counsel-flycheck)



