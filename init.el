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

(package-initialize)

(setq xjdr-packages
  '(
    cheatsheet
    editorconfig
    google-c-style
    ido-vertical-mode
    java-imports
    javadoc-lookup
    smex
    whole-line-or-region
    ))

(defun xjdr-bootstrap-packages ()
  (package-refresh-contents)
  (dolist (list-item xjdr-packages)
    (unless (package-installed-p list-item)
      (package-install list-item))))

; bootstrap packages
(require 'cl-extra)
(when (cl-some (lambda (item) (not (package-installed-p item))) xjdr-packages)
  (xjdr-bootstrap-packages))

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
(add-hook 'term-mode-hook
  (lambda()
    (setq-local show-trailing-whitespace nil)))

;; Custom key bindings
(global-set-key (kbd "s-<return>") 'toggle-frame-fullscreen)
;; Not sure why we're doing this, it probably belongs in a mode hook
;;(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "s-/") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-c C-k") 'compile)
(global-set-key (kbd "C-c t r") (lambda () (interactive) (compile "make -k test")))
(global-set-key (kbd "C-x t") 'ansi-term)
(global-set-key (kbd "C-x c e") 'flymake-display-err-menu-for-current-line)
(global-set-key (kbd "C-x c n") 'flymake-goto-next-error)
(global-set-key (kbd "C-x c p") 'flymake-goto-prev-error)
;; emacs binds this to M-space by default, which unfortunately Alfred binds to
(global-set-key (kbd "s-\\") 'just-one-space)

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

(org-babel-do-load-languages
 'org-babel-load-languages
 '((ditaa . t)))

(custom-set-variables
 '(org-ditaa-jar-path "/usr/local/Cellar/ditaa/0.9/libexec/ditaa0_9.jar"))

(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-buffer)

;; Flymake
(defun flymake-display-warning (warning)
  "Display a warning to the user, using lwarn"
  (message warning))

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
(require 'javadoc-lookup)
(javadoc-add-artifacts [io.netty netty-all 4.1.1.Final])
(javadoc-add-artifacts [junit junit 4.12])
(javadoc-add-artifacts [org.apache.thrift libthrift 0.9.3])

(require 'java-imports)

(setq java-imports-find-block-function 'java-imports-find-place-sorted-block)

(require 'autoinsert)
(require 'skeleton)
(setq java-boilerplate-skeleton '(nil
  '(text-mode)
  "package "
  (replace-regexp-in-string "/" "."
    (replace-regexp-in-string "/$" ""
      (elt (split-string (file-name-directory (buffer-file-name)) "java/") 1)))
  ";" \n \n
  "public class " (file-name-base (buffer-file-name)) " {" \n \n
  "  " _ \n
  "  public " (file-name-base (buffer-file-name)) "() {" \n
  "}" \n
  \n
  "}" \n
  '(java-mode)))

(define-auto-insert
  '("\\.java\\'" . "Java skeleton")
  java-boilerplate-skeleton)

(defun java-test-name (test-kind)
  "convert from java implementation name to test name"
  (let ((basename (replace-regexp-in-string "main/java" "test/java" (file-name-sans-extension (buffer-file-name)))))
    (concat basename (if test-kind (symbol-name test-kind) "") "Test.java")))

(defun java-impl-name ()
  "convert from java test name to implementation"
  (concat
    (replace-regexp-in-string "FunctionalTest\\|IntegrationTest\\|UnitTest" ""
      (replace-regexp-in-string "test/java" "main/java"
        (file-name-sans-extension (buffer-file-name)))) ".java"))

(defun java-open-functional-test ()
  (interactive)
  (find-file (java-test-name 'Functional)))

(defun java-open-generic-test ()
  (interactive)
  (find-file (java-test-name nil)))

(defun java-open-integration-test ()
  (interactive)
  (find-file (java-test-name 'Integration)))

(defun java-open-unit-test ()
  (interactive)
  (find-file (java-test-name 'Unit)))

(defun java-open-implementation ()
  (interactive)
  (find-file (java-impl-name)))

(c-add-style "custom-java"
  '("java"
    (c-basic-offset 2)
    (c-offsets-alist
      (arglist-intro . +)
      (arglist-close . 0)
      (statement-cont . +)
      (inexpr-class . 0)
    )))

(setq java-use-infer nil)

(setq java-mode-hook nil)
(add-hook 'java-mode-hook
          (lambda ()
            (setq-local compilation-environment (list
              (concat "FILE_NAME=" (buffer-file-name))
              (concat "NOINFER=" (if java-use-infer "" "1"))))
            (editorconfig-mode)
            (flymake-mode)
            (java-imports-scan-file)
            (subword-mode)
            (c-set-style "custom-java")
            (define-key java-mode-map (kbd "C-c i") 'java-imports-add-import-dwim)
            (define-key java-mode-map (kbd "C-c t f") 'java-open-functional-test)
            (define-key java-mode-map (kbd "C-c t g") 'java-open-generic-test)
            (define-key java-mode-map (kbd "C-c t i") 'java-open-integration-test)
            (define-key java-mode-map (kbd "C-c t t") 'java-open-implementation)
            (define-key java-mode-map (kbd "C-c t u") 'java-open-unit-test)
            ))

;; Python
(setq python-shell-interpreter "/usr/local/bin/ipython"
     python-shell-interpreter-args "-i")
(add-hook 'python-mode-hook
                    (lambda ()
            (flymake-mode)
            (editorconfig-mode)))

;; Theme?
(custom-set-faces
 '(default ((t (:inherit nil :stipple nil :background "Black" :foreground "White"))))
 '(font-lock-warning-face ((nil (:foreground "#ff6666"))))
 '(highlight ((((class color) (min-colors 88) (background dark)) (:background "#111111"))))
 '(hl-line ((nil (:background "#222222"))))
 '(region ((nil (:background "#255255"))))
 '(show-paren-match ((nil (:background "#1793d0"))))
 '(show-paren-mismatch ((((class color)) (:background "red")))))

(set-face-attribute 'mode-line nil
   :foreground "#1793d0"
   :background "#111111"
   :box '(:line-width 6 :color "#111111" :style nil))

(set-face-attribute 'mode-line-inactive nil
   :foreground "#1793d0"
   :background "#222222"
   :box '(:line-width 6 :color "#22222" :style nil))

;; Info
(add-hook 'Info-mode-hook		; After Info-mode has started
  (lambda ()
    (add-to-list 'Info-additional-directory-list "~/.emacs.d/info")))

(require 'org)
(org-babel-load-file (expand-file-name "org-init/emacs-cheatsheet.org" user-emacs-directory))

(let ((custom-file (expand-file-name "custom.el" user-emacs-directory)))
  (if (file-exists-p custom-file)
    (load custom-file)))
