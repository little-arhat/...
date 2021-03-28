(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-parentheses-colors '("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900"))
 '(safe-local-variable-values
   '((eval progn
           (add-to-list 'cider-jack-in-nrepl-middlewares "shadow.cljs.devtools.server.nrepl/middleware")
           (cider-register-cljs-repl-type 'procflow "(user/shadow-cljs-repl)")
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
