(use-package cider
  :ensure t
  :config
  (add-hook 'cider-repl-mode-hook 'paredit-mode)

  ;; treat Schema defn/fn as regular defn/fn
  (put-clojure-indent 's/defn :defn)
  (put-clojure-indent 's/fn :defn)
  (put 's/defn 'clojure-doc-string-elt 2)
  (put 's/defschema 'clojure-doc-string-elt 2)

  (defun cider-repl-reset ()
    (interactive)
    (save-some-buffers)
    (cider-interactive-eval
     (concat "((or (resolve 'user/reset)"
	     "     (resolve 'clojure.tools.namespace.repl/refresh)))")))

  (define-key cider-mode-map (kbd "C-c C-x") 'cider-repl-reset)
  (define-key clojure-mode-map (kbd "C-c C-x") 'cider-repl-reset))

(use-package clojure-cheatsheet
  :ensure t)

(use-package clojure-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
  (add-hook 'clojure-mode-hook 'paredit-mode))

(use-package clojure-mode-extra-font-locking
  :ensure t)


(provide 'clojure)
