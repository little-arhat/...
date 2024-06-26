;; various small modes configuration

(setq tags-file-name (expand-file-name "~/TAGS"))
;; dired
(setq
 dired-bind-jump nil
 dired-omit-extensions '(".pyc" ".elc")
 dired-omit-files "^\\.?#\\|^\\.")
(add-hook 'dired-mode-hook 'dired-omit-mode)

(setq eshell-scroll-to-bottom-on-input 'all
      eshell-error-if-no-glob t
      eshell-hist-ignoredups t
      eshell-save-history-on-exit t
      eshell-prefer-lisp-functions nil)

(column-number-mode 1)
(transient-mark-mode 1)
(setq mark-even-if-inactive t)
(show-paren-mode 1)
(global-subword-mode 1)
(setq save-abbrevs nil)

(which-function-mode)

;; TODO: highilight TODO et al for al modes

(require 'linum)
(set-face-attribute 'linum nil
                    :background "gray"
                    :foreground "red")
(add-hook 'write-file-functions 'delete-trailing-whitespace)

;; I hate blinking
(if (fboundp 'blink-cursor-mode)
  (blink-cursor-mode -1))

;; It's rare, but annoying
(if (fboundp 'global-semantic-stickyfunc-mode)
    (global-semantic-stickyfunc-mode -1))

(winner-mode 1) ;; window configuration undo/redo

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; elisp
(defun lambda-elisp-mode-hook ()
  (font-lock-add-keywords
   nil `(("\\<lambda\\>"
          (0 (progn (compose-region (match-beginning 0) (match-end 0)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))
(add-hook 'emacs-lisp-mode-hook 'lambda-elisp-mode-hook)
;; VC mode, do not ever annoy me with slow file loading time
(remove-hook 'find-file-hook 'vc-find-file-hook)
