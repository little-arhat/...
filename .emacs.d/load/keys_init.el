;; unsets
(global-unset-key (kbd "C-q"))

(global-set-key (kbd "C-x C-b") 'bs-show)
(global-set-key (kbd "C-,") 'bs-show)

(global-set-key (kbd "C-x C-z") 'undo)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-?") 'help-command)
(global-set-key (kbd "C-x C-r") 'query-replace-regexp)

(global-set-key (kbd "C-x C-d") 'dired-jump)

(global-set-key (kbd "C-c k") (fun-for-bind kill-buffer nil))
(global-set-key (kbd "C-M-l") (fun-for-bind switch-to-buffer (other-buffer)))
(global-set-key (kbd "C-M-z") (lambda (&optional arg char)
                                (interactive "p\ncZap backward to char: ")
                                (zap-to-char (- arg) char)))
(global-set-key (kbd "M-<up>") 'prh:move-line-up)
(global-set-key (kbd "M-<down>") 'prh:move-line-down)
(global-set-key (kbd "C-M-<up>") 'prh:duplicate-line-up)
(global-set-key (kbd "C-M-<down>") 'prh:duplicate-line-down)
(global-set-key (kbd "<home>") 'dev-studio-beginning-of-line)
(global-set-key (kbd "M-Y") (lambda (&optional arg)
                              (interactive "*p")
                              (yank-pop (- arg))))
(autoload 'kill-ring-search "kill-ring-search"
  "Search the kill ring in the minibuffer."
  (interactive))
(global-set-key (kbd "C-M-y") 'kill-ring-search)
(global-set-key (kbd "C-d") 'delete-forward-char)
;; windows
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-3") 'split-window-horizontally)
(global-set-key (kbd "M-0") 'delete-window)

;; buffers
(global-set-key (kbd "<up>")     'windmove-up)
(global-set-key (kbd "<down>")   'windmove-down)
(global-set-key (kbd "<left>")   'windmove-left)
(global-set-key (kbd "<right>")  'windmove-right)

(global-set-key (kbd "C-q C-q") 'rotate-windows)
(global-set-key (kbd "C-x C-l") 'bs-show)
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)

;; comments
(global-set-key (kbd "C-c C-o") 'comment-region)
;; need this for compatibility with go-oracle
(global-set-key (kbd "C-x C-o") 'comment-region)
(global-set-key (kbd "C-c C-y") 'uncomment-region)

;; misc
(global-set-key (kbd "C-c \"") 'insert-quotes)

(global-set-key (kbd "C-c C-c") 'compile)


(global-set-key (kbd "C-c C-k") 'my-pbcopy)


(global-set-key (kbd "C-c C-p") 'my-pbpaste)

(global-set-key (kbd "C-c C-s") 'search-from-clipboard)

;; error
;; (global-set-key (kbd "C-c C-x") 'next-error)
