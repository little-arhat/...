(require 'prh-bufsw)
(setq stesla-hated-buffer-regexps '("^ " "*Buffer" 
                                    "*Scratch*"
                                    "^\\*trace"
                                    "^\\*tramp"
                                    "^\\*"
                                    "*Messages*"
                                    "*Completions*"))
(setq pc-bufsw::quite-time 1)

;;; (C) banister (John Mair)
;;; slightly patched by me ^_^ 
;;; http://github.com/banister/window-rotate-for-emacs
(defun rotate-windows-helper(x d)
  (if (equal (cdr x) nil) (set-window-buffer (car x) d)
    (set-window-buffer (car x) (window-buffer (cadr x))) (rotate-windows-helper (cdr x) d)))
 
(defun rotate-windows ()
  (interactive)
  (rotate-windows-helper (window-list)
                         (window-buffer
                          (car (window-list)))))

