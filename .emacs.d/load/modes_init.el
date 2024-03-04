;; various small modes configuration

(require 'warnings)

(use-package imenu-anywhere
  :ensure t
  :commands imenu-anywhere
  :bind ("C-c M-A" . imenu-anywhere))

(require 'imenu)
(setq imenu-auto-rescan t)

(use-package flimenu
  :ensure t
  :config
  (flimenu-global-mode))

(setq tags-file-name (expand-file-name "~/TAGS"))
;; dired
(setq
 dired-bind-jump nil
 dired-omit-extensions '(".pyc" ".elc")
 dired-omit-files "^\\.?#\\|^\\.")
(autoload 'dired-jump "dired-x" "Jump to dir of current file" t)
(autoload 'dired-omit-mode "dired-x" "Omit unnecessary files in dired view" t)
(add-hook 'dired-mode-hook 'dired-omit-mode)
(add-hook 'dired-omit-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "M-o") nil)
            (define-key dired-mode-map (kbd "M-O") 'dired-omit-mode)
            (define-key dired-mode-map (kbd "C-,")
              (lambda () (bs--show-with-configuration "dired")))))

(setq eshell-scroll-to-bottom-on-input 'all
      eshell-error-if-no-glob t
      eshell-hist-ignoredups t
      eshell-save-history-on-exit t
      eshell-prefer-lisp-functions nil)

(column-number-mode 1)
(transient-mark-mode 1)
(setq mark-even-if-inactive t)
(show-paren-mode 1)
(setq rainbow-delimiters-highlight-braces-p nil)
(global-subword-mode 1)


;; saving history
(savehist-mode 1)
(setq savehist-additional-variables
      '(search-ring
        regexp-search-ring
        kill-ring
        file-name-history
        command-history
        shell-command-history))
(setq save-abbrevs nil)

(which-function-mode)

;; whitespaces
(require 'whitespace)
(setq whitespace-style '(face trailing tabs lines-tail))
(setq whitespace-line-column 80)
;; face for long lines' tails
(set-face-attribute 'whitespace-line nil
                    :background "red1"
                    :foreground "yellow"
                    :weight 'bold)
;; face for Tabs
(set-face-attribute 'whitespace-tab nil
                    :background "red1"
                    :foreground "yellow"
                    :weight 'bold)

;; TODO: highilight TODO et al for al modes

(require 'linum)
(set-face-attribute 'linum nil
                    :background "gray"
                    :foreground "red")

(defvar hooks-with-whitespaces
      '(erlang-mode-hook
        haskell-mode-hook
        tuareg-mode-hook
        ruby-mode-hook
        python-ts-mode-hook
        clojure-mode-hook
        rust-ts-mode-hook
        js-mode-hook))
(dolist (hook hooks-with-whitespaces) (add-hook hook 'whitespace-mode))
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

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


;; ;; python

;; (eval-after-load "python"
;;   '(progn
;;      (define-key python-mode-map (kbd "RET") 'newline-maybe-indent)
;;      (global-set-key (kbd "M-RET e p") 'python-shell-send-defun)
;;      (global-set-key (kbd "M-RET e r") 'python-shell-send-region)
;;      (subword-mode)))
;; (setq python-shell-interpreter "python3")

;; ;; Ruby

;; (eval-after-load "ruby-mode"
;;   '(progn
;;      (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)))

;; ;; Javascript

;; (eval-after-load "js"
;;   '(progn
;;      (define-key js-mode-map (kbd "RET") 'newline-maybe-indent)))

;; ;; ObjC
;; (add-hook 'objc-mode-hook
;;           (lambda ()
;;             (setq comment-start "/*")
;;             (setq comment-end "*/")
;;             (subword-mode)
;;             (setq tab-width 8)
;;             (setq c-basic-offset 8)
;;             (c-set-offset 'substatement-open 0)))
;; (add-hook 'c-mode-hook
;;           (lambda ()
;;             (setq comment-start "/*")
;;             (setq comment-end "*/")
;;             (subword-mode)
;;             (setq tab-width 4)
;;             (setq c-basic-offset 4)
;;             (c-set-offset 'substatement-open 0)))

;; (add-hook 'cpp-mode-hook
;;           (lambda ()
;;             (global-set-key (kbd "C-c C-o") 'comment-region)
;;             (global-set-key (kbd "C-c C-y") 'uncomment-region)
;;             (setq comment-start "/*")
;;             (setq comment-end "*/")
;;             (subword-mode)
;;             (setq tab-width 4)
;;             (setq c-basic-offset 4)
;;             (c-set-offset 'substatement-open 0)))
;; ;; lisp

;; (add-hook 'lisp-mode-hook
;;           (lambda ()
;;             (setq lisp-indent-function 2)
;;             (setq lisp-indent-offset 2)))
