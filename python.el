;; Python
(setq python-shell-interpreter "/usr/local/bin/ipython"
      python-shell-interpreter-args "-i")

(add-hook 'python-mode-hook
	  (lambda ()
	    (python-mode)
	    (flycheck-mode)
	    (editorconfig-mode)))

(provide 'python)
