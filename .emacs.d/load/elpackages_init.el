
;; org mode
(use-package org
  :bind (:map org-mode-map
              ("C-," . nil)
              ("C-S-<up>" . org-timestamp-up)
              ("C-S-<down>" . org-timestamp-down))
  :config
  (setq org-hide-leading-stars t))

;; major modes
(use-package markdown-mode
  :ensure t
  :mode ("\\.markdown\\'" . markdown-mode)
        ("\\.md\\'" . markdown-mode)
  :bind (:map markdown-mode-map
         ("M-<right>" . nil)
         ("M-<left>" . nil)))

(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :commands godoc-gogetdoc
  :config
  (add-hook 'go-mode-hook
            (lambda ()
              (set (make-local-variable 'whitespace-style)
                   '(face trailing lines-tail))
              (whitespace-mode -1)
              (whitespace-mode 1)))
  :custom
  (godoc-at-point-function #'godoc-gogetdoc))

(defvar python-mode-map)
(add-hook 'python-mode-hook
          (lambda ()
            (setq imenu-create-index-function 'python-imenu-create-index)
            (define-key python-mode-map (kbd "RET") 'newline-maybe-indent)))


(use-package flycheck
  :ensure t
  :bind (("C-c C-x" . flycheck-next-error))
  :init
  (add-hook 'after-init-hook 'global-flycheck-mode))

(use-package flycheck-pyflakes
  :ensure t
  :init
  (add-hook 'python-mode-hook (lambda () (require 'flycheck-pyflakes))))

(use-package flycheck-joker
  :ensure t)

;; Ruby
(defvar ruby-mode-map)
(with-eval-after-load 'ruby-mode
  (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent))

;; highlight parentheses
(use-package highlight-parentheses
  :ensure t
  :init
  (dolist (hook '(python-mode-hook
                  emacs-lisp-mode-hook
                  clojure-mode-hook))
    (add-hook hook 'highlight-parentheses-mode)))

;; whole-line-or-region
(use-package whole-line-or-region
  :ensure t
  :commands
  whole-line-or-region-global-mode
  whole-line-or-region-call-with-region
  :init
  (defun whole-line-kill-region-or-word-backward (prefix)
    "Kill (cut) region or just a single word backward"
    (interactive "*p")
    (if (not (and mark-active (/= (point) (mark))))
        (subword-backward-kill prefix)
      (whole-line-or-region-call-with-region 'kill-region prefix t)))
  (setq whole-line-or-region-extensions-alist
        '((copy-region-as-kill whole-line-or-region-copy-region-as-kill nil)
          (kill-region whole-line-kill-region-or-word-backward nil)
          (kill-ring-save whole-line-or-region-kill-ring-save nil)
          (yank whole-line-or-region-yank nil)))
  (whole-line-or-region-global-mode))


(use-package clojure-mode
  :ensure t
  :commands put-clojure-indent
  :mode ("\\.boot\\'" . clojure-mode)
        ("\\.edn\\'" . clojure-mode)
  :init
  (setq clojure-indent-style :always-indent)
  (setq clojure-thread-all-but-last t)
  (setq clojure-align-forms-automatically t)
  (setq clojure-toplevel-inside-comment-form t))

(use-package cider
  :ensure t
  :no-require t
  :commands cider-mode
  :bind (:map cider-mode-map
              ("C-c C-f" . nil)
         :map cider-repl-mode-map
              ("C-c M-r" . cider-repl-previous-matching-input)
              ("C-c M-s" . cider-repl-next-matching-input))
  :init
  (add-hook 'clojure-mode-hook 'cider-mode)
  (add-hook 'cider-repl-mode-hook 'smartparens-mode)
  :config
  (setq cider-repl-history-file "~/.emacs.d/cider-history"
        ;cider-cljs-repl "(do (require '[figwheel-sidecar.repl-api :as ra]) (ra/cljs-repl))"
        cider-repl-display-help-banner nil))

(use-package clj-refactor
  :ensure t
  :init
  (add-hook 'clojure-mode-hook
            '(lambda ()
               (clj-refactor-mode 1)
               (cljr-add-keybindings-with-prefix "C-c C-m"))))

(use-package smartparens
  :ensure t
  :no-require t
  :commands smartparens-global-mode
  :init
  (require 'smartparens-config)
  (show-smartparens-global-mode t)
  (show-paren-mode nil)
  (add-hook 'lisp-mode-hook 'smartparens-strict-mode)
  (add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)
  (add-hook 'clojure-mode-hook 'smartparens-strict-mode)
  :bind (:map smartparens-mode-map
              ("C-M-f" . sp-forward-sexp)
              ("C-M-b" . sp-backward-sexp)
              ("C-M-d" . sp-down-sexp)
              ("C-M-e" . sp-up-sexp)
              ("C-S-a" . sp-beginning-of-sexp)
              ("C-S-d" . sp-end-of-sexp)
              ("C-M-n" . sp-next-sexp)
              ("C-M-p" . sp-previous-sexp)
              ("C-c (" . sp-add-to-next-sexp)
              ("C-c )" . sp-add-to-previous-sexp)
              ("C-M-k" . sp-kill-sexp)
              ("C-k" . sp-kill-hybrid-sexp)
              ("C-M-w" . sp-copy-sexp)
              ("M-<delete>" . sp-unwrap-sexp)
              ("M-<backspace>" . sp-backward-unwrap-sexp)
              ("M-s" . sp-splice-sexp)
              ("M-c" . sp-copy-sexp)
              ("C-c c" . sp-comment))
  :init
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

(use-package sql-indent
  :ensure t
  :commands sqlind-minor-mode
  :init
  (add-hook 'sql-mode-hook 'sqlind-minor-mode))

;; lsp

(use-package lsp-mode
  :ensure t
  :hook ((go-mode python-mode rust-mode java-mode scala-mode) . lsp-deferred)
  :config
  (setq lsp-auto-execute-action nil))

;; rust
(use-package rust-mode
  :ensure t
  :init
  (setq rust-format-on-save t)
  (setq indent-tabs-mode nil)
  :bind (:map rust-mode-map
              ("C-c C-c" . rust-run))
  :config  (use-package lsp-rust
             :ensure lsp-mode
             :bind
             (("M-c" . lsp-extend-selection)
              ("M-j" . lsp-rust-analyzer-join-lines)
              ("C-." . lsp-goto-type-definition))
             :custom
             (lsp-rust-server (if (executable-find "rust-analyzer") 'rust-analyzer 'rls))
             (lsp-rust-analyzer-server-display-inlay-hints nil)))

(use-package cargo  :ensure t)

(use-package flycheck-rust
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  (add-hook 'rust-mode-hook #'flycheck-mode))

;; ocaml

(use-package tuareg
  :mode (("\\.ml[ily]?$" . tuareg-mode)
         ("\\.topml$" . tuareg-mode))
  :ensure t
  :init
  (progn
    ;; Make OCaml-generated files invisible to filename completion
    (dolist (ext '(".cmo" ".cmx" ".cma" ".cmxa" ".cmi" ".cmxs" ".cmt" ".cmti" ".annot"))
      (add-to-list 'completion-ignored-extensions ext))))

(use-package utop
  :ensure t
  :init
  (progn
    (add-hook 'tuareg-mode-hook 'utop-minor-mode))
  :config
  (progn
    (if (executable-find "opam")
        (setq utop-command "opam config exec -- utop -emacs")
      ))
  (define-key utop-mode-map (kbd "C-j") 'utop-history-goto-next)
  (define-key utop-mode-map (kbd "C-k") 'utop-history-goto-prev))

(use-package merlin
  :ensure t
  :init
  (progn
    (add-hook 'tuareg-mode-hook 'merlin-mode)
    (setq merlin-completion-with-doc t)))

;; Various
(use-package projectile
  :ensure t
  :commands projectile-mode
  :init
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'default)
  (projectile-mode 1)
  :bind (("C-x b" . projectile-switch-to-buffer)
         ("C-c i" . projectile-find-other-file)
         :map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(use-package ctrlf
  :ensure t
  :config
  (ctrlf-mode 1))

(use-package selectrum
  :ensure t
  :config
  (selectrum-mode 1))

(use-package selectrum-prescient
  :ensure t
  :config
  (selectrum-prescient-mode)
  (prescient-persist-mode))

(use-package deadgrep
  :ensure t
  :commands (deadgrep--read-search-term)
  :bind (("C-c s" . deadgrep)))

(use-package imenu-anywhere
  :ensure t
  :commands imenu-anywhere
  :bind ("C-c M-A" . imenu-anywhere))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch))
  :config
  (setq magit-save-repository-buffers nil)
  (defun magit-rebase-origin-master (args)
    (interactive (list (magit-rebase-arguments)))
    (message "Rebasing...")
    (magit-git-rebase "origin/master" args)
    (message "Rebasing...done"))
  (transient-append-suffix 'magit-rebase "e" '("o" "origin/master" magit-rebase-origin-master)))

(use-package string-inflection
  :ensure t)

(use-package piu
  :bind (("C-x p" . piu)))

(use-package dockerfile-mode
  :ensure t
  :no-require t
  :commands dockerfile-mode
  :init (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package hcl-mode
  :ensure t)

(use-package terraform-mode
  :ensure t)

(use-package lua-mode
  :ensure t)

(use-package q-mode
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :commands rainbow-delimiters-mode-enable)

(use-package flyspell
  :ensure t
  :custom
  (ispell-program-name "aspell")
  (ispell-dictionary "en_GB")
  (ispell-silently-savep t)
  ;; --sug-mode=ultra is a suggestion mode, ultra gives the best candidates (but
  ;; is the slowest).
  ;; --run-together can check camel case words. Also aspell process with this
  ;; option can be reused.
  (ispell-extra-args '("--sug-mode=ultra" "--run-together"))
  ;; Do not show welcome message when flyspell starts.
  (flyspell-issue-welcome-flag nil)
  ;; Do not show messages when checking words.
  (flyspell-issue-message-flag nil)
  :bind
  (:map flyspell-mode-map
        ("C-;" . nil)
        ("C-." . nil)
        ("C-," . nil))
  :hook
  (((text-mode outline-mode) . flyspell-mode)
   ;; Check comments and strings in source code.
   (prog-mode . flyspell-prog-mode)
   (before-save-hook . flyspell-buffer)))

(use-package solarized-theme
  :ensure t)

(use-package company
  :ensure t)

(use-package telega
  :ensure t
  :bind-keymap ("C-c t" . telega-prefix-map)
  :commands (telega)
  :config
  (use-package telega-mnz)
  (add-hook 'telega-load-hook 'global-telega-squash-message-mode)
  (add-hook 'telega-load-hook 'global-telega-mnz-mode)
  (add-hook 'telega-load-hook 'company-mode)
  (setq telega-vvnote-video-cmd
        "ffmpeg -f avfoundation -s 640x480 -framerate 30 -i default -r 30 -f avfoundation -i :default -vf format=yuv420p,crop=240:240:240:0 -vcodec hevc -vb 300k -strict -2 -acodec opus -ac 1 -ab 32k"))
