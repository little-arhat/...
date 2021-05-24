(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-parentheses-colors '("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900"))
 '(package-selected-packages
   '(flyspell lsp-treemacs borg lsp-rust whole-line-or-region utop use-package tuareg terraform-mode telega string-inflection sql-indent solarized-theme smartparens selectrum-prescient rainbow-delimiters q-mode projectile merlin magit lua-mode lsp-mode kill-ring-search imenu-anywhere highlight-parentheses go-mode flycheck-rust flycheck-pyflakes flycheck-joker flimenu dockerfile-mode deadgrep dash-functional ctrlf company clj-refactor cargo))
 '(safe-local-variable-values
   '((cider-ns-refresh-after-fn . "integrant.repl/resume")
     (cider-ns-refresh-before-fn . "integrant.repl/suspend")
     (eval progn
           (add-to-list 'cider-jack-in-nrepl-middlewares "shadow.cljs.devtools.server.nrepl/middleware")
           (defun cider-quit
               (&optional repl)
             (interactive)
             (cl-loop for repl in
                      (cider-repls 'multi)
                      do
                      (cider--close-connection repl))
             (unless
                 (cider-sessions)
               (cider-close-ancillary-buffers))
             (run-hooks 'sesman-post-command-hook)))
     (cider-repl-display-help-banner)
     (encoding . utf-8)
     (prompt-to-byte-compile))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
