;;  init.el
;;  xjdr - here be dragons and whatnot

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

;; UI
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

;;; Code:
(setq package-archives
      (append package-archives
	      '(("melpa" . "http://melpa.milkbox.net/packages/"))))

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

;; Custom Key Bindings
(global-set-key (kbd "s-<return>") 'toggle-frame-fullscreen)
(global-set-key (kbd "s-/") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-c C-c") 'compile)
(global-set-key (kbd "C-c C-t") (lambda () (interactive) (compile "make -k test")))

;; Misc
(show-paren-mode)

;;;; Tramp is good
(require 'tramp)
(setq tramp-default-method "ssh")
(setq tramp-persistency-file-name (emacs-d "var/tramp-history.el"))

;; eshell stuffs
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

(setq eshell-prompt-function (lambda nil
			       (concat
				(propertize (eshell/pwd) 'face `(:foreground "#A074C4"))
				(propertize " $ " 'face `(:foreground "black")))))
(setq eshell-highlight-prompt nil)

(fset 'eshell-on
      "\C-x1\M-xeshell\n")
(fset 'eshell-off
      "\C-x3\M-xbury-buffer\n\C-xo\M-xbury-buffer\n\M-xwindmove-left")

(defun toggle-eshell ()
  (interactive)
  (if (string= "eshell-mode" (eval 'major-mode))
      (execute-kbd-macro (symbol-function 'eshell-off))
    (execute-kbd-macro (symbol-function 'eshell-on))))

(global-set-key (kbd "C-x t") 'toggle-eshell)

;; Org
(setq ispell-program-name "/usr/local/bin/aspell")
(setq ispell-dictionary "english")
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-buffer)
(add-hook 'org-mode-hook 'visual-line-mode t)
(org-babel-do-load-languages 'org-babel-load-languages
			     '((sh         . t)
			       (C           .t)
			       (java       . t)
			       (js         . t)
			       (emacs-lisp . t)
			       (ditaa      . t)
			       (scala      . t)
			       (clojure    . t)
			       (python     . t)))

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

;; C++ Stuffs
(use-package google-c-style
  :ensure t)

(defun my-c++-mode-hook ()
  (google-set-c-style)
  (google-make-newline-indent))

(add-hook 'c++-mode-hook
	  (lambda ()
	    (flycheck-mode)
	    (my-c++-mode-hook)
	    (editorconfig-mode)))

;; Java Stuffs
(use-package javadoc-lookup
  :ensure t
  :config
  (javadoc-add-artifacts [io.netty netty-all 4.1.11.Final])
  (javadoc-add-artifacts [com.google.guava guava 21.0])
  (javadoc-add-artifacts [junit junit 4.12])
  (javadoc-add-artifacts [org.apache.thrift libthrift 0.9.3])
  )


(use-package java-imports
  :ensure t
  :config
  (setq java-imports-find-block-function 'java-imports-find-place-sorted-block)
  )

;; Java
(add-hook 'java-mode-hook (lambda ()
			    (setq c-basic-offset 2)
			    (setq tab-width 2)
			    (setq indent-tabs-mode t)
			    (setq fill-column 100)
			    (fci-mode t)
			    (subword-mode t)
			    (local-set-key (kbd "C-M-h") 'windmove-left)
			    (hs-minor-mode 1)))


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
   (replace-regexp-in-string "FunctionalTest\\|IntegrationTest\\|UnitTest\\|Test" ""
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

(require 'dash)

(flycheck-define-checker infer
  "A Java syntax and style checker using infer.
See URL http://fbinfer.com/"
  :command ("infer" "--" "mvn" "-f"
	    (eval (-> (projectile-project-root)
		      (concat "pom.xml")
		      (expand-file-name)))
	    "compile")
  :error-patterns
  ((error line-start (file-name) ":" line ":" " error:" (message) line-end)
   (warning line-start (file-name) ":" line ":" " warning:" (message) line-end)
   (info line-start (file-name) ":" line ":" " info:" (message) line-end))
  :modes java-mode)


(flycheck-define-checker mvn
  "A Maven Java synax checker."
  :command ("mvn" "-f"
	    (eval (-> (projectile-project-root)
		      (concat "pom.xml")
		      (expand-file-name)))
	    "compile")
  :error-patterns ((error line-start "[ERROR] " (file-name) ":[" line "," column "]"
			  (message) line-end)
		   (warning line-start "[WARNING] " (file-name) ":[" line "," column "]"
			    (message) line-end))
  :modes java-mode)

(setq java-mode-hook nil)
(add-hook 'java-mode-hook
	  (lambda ()
	    (setq-local compilation-environment (list
						 (concat "FILE_NAME=" (buffer-file-name))
						 (concat "NOINFER=" (if java-use-infer "" "1"))))
	    (editorconfig-mode)
	    (flycheck-mode)
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
	    (python-mode)
	    (flycheck-mode)
	    (editorconfig-mode)))


;; mode line sexy maker
(set-cursor-color "#d33682")
(set-face-attribute 'mode-line nil
		    :background "#FFFFEA"
		    :box '(:line-width 2 :color "black" :style nil))
(set-face-attribute 'mode-line-inactive nil
		    :background "#FFFFEA"
		    :box '(:line-width 4 :color "#FFFFEA" :style nil))

;; Fonts and whatnot
(set-face-attribute 'default nil
					;		      :family "DejaVu Sans Mono"
		    :height 140
		    :weight 'Light
		    :width 'normal)

;; Colors (or lack thereof)
(let ((blue "#55B5DB")
      (green "#9FCA56")
      (yellow "#DCCD69")
      (red "#CE4045")
      (dark-blue "#4F99D3")
      (intense-green "#8BE03C"))

  (custom-set-faces
   ;; Fancy Colors
   `(default ((t (:background ,"#FFFFEA"))))
   `(error ((t (:foreground ,red :weight bold :underline (:color ,red :style line)))))
   `(isearch ((t (:background ,"white" :foreground ,"blue" :box (:line-width 1 :color ,dark-blue) :weight bold))))
   `(lazy-highlight ((t (:background ,"white" :foreground ,"#858D8A" :box (:line-width 1 :color ,dark-blue)))))
   `(secondary-selection ((t (:background ,"grey"))))
   `(trailing-whitespace ((t (:background ,"grey"))))
   `(match ((t (:weight bold :foreground ,"white" :background ,intense-green))))
   `(next-error ((t (:inherit (region)))))
   `(query-replace ((t (:inherit (isearch)))))

   ;; Font Lock
   `(font-lock-builtin-face ((t (:foreground ,"black"))))
   `(font-lock-warning-face ((t (:foreground , "red" :weight bold))))
   `(font-lock-comment-delimiter-face ((t (:inherit (font-lock-comment-face)))))
   `(font-lock-comment-face ((t (:foreground ,"#708090")))) ;; Perhaps this should be black too?
   `(font-lock-constant-face ((t (:foreground ,"black"))))
   `(font-lock-doc-face ((t (:foreground ,"black"))))
   `(font-lock-function-name-face ((t (:foreground ,"black"))))
   `(font-lock-keyword-face ((t (:foreground ,"#3F00FF"))))
   `(font-lock-negation-char-face ((t nil)))
   `(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
   `(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
   `(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
   `(font-lock-string-face ((t (:foreground ,"#555555")))) ;; Perhaps this should just be black as well?
   `(font-lock-type-face ((t (:foreground ,"black"))))
   `(font-lock-variable-name-face ((t (:foreground ,"black"))))
   ))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (projectile markdown-mode flycheck-java-maven use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "#FFFFEA"))))
 '(error ((t (:foreground "#CE4045" :weight bold :underline (:color "#CE4045" :style line)))))
 '(font-lock-builtin-face ((t (:foreground "black"))))
 '(font-lock-comment-delimiter-face ((t (:inherit (font-lock-comment-face)))))
 '(font-lock-comment-face ((t (:foreground "#708090"))))
 '(font-lock-constant-face ((t (:foreground "black"))))
 '(font-lock-doc-face ((t (:foreground "black"))))
 '(font-lock-function-name-face ((t (:foreground "black"))))
 '(font-lock-keyword-face ((t (:foreground "#3F00FF"))))
 '(font-lock-negation-char-face ((t nil)))
 '(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
 '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
 '(font-lock-string-face ((t (:foreground "#555555"))))
 '(font-lock-type-face ((t (:foreground "black"))))
 '(font-lock-variable-name-face ((t (:foreground "black"))))
 '(font-lock-warning-face ((t (:foreground "red" :weight bold))))
 '(isearch ((t (:background "white" :foreground "blue" :box (:line-width 1 :color "#4F99D3") :weight bold))))
 '(lazy-highlight ((t (:background "white" :foreground "#858D8A" :box (:line-width 1 :color "#4F99D3")))))
 '(match ((t (:weight bold :foreground "white" :background "#8BE03C"))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch)))))
 '(secondary-selection ((t (:background "grey"))))
 '(trailing-whitespace ((t (:background "grey")))))
