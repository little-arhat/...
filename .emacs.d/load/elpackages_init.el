;;; package -- summary

;; org mode.
(use-package org
  :defer t
  :bind (:map org-mode-map
              ("C-," . nil)
              ("C-S-<up>" . org-timestamp-up)
              ("C-S-<down>" . org-timestamp-down))
  :config
  (setq org-hide-leading-stars t))

;; major modes
(use-package markdown-mode
  :defer t
  :mode ("\\.markdown\\'" . markdown-mode)
        ("\\.md\\'" . markdown-mode)
  :bind (:map markdown-mode-map
         ("M-<right>" . nil)
         ("M-<left>" . nil)))

(use-package go-mode
  :defer t
  :mode ("\\.go\\'" . go-mode)
  :commands godoc-gogetdoc
  :config
  (add-hook 'go-mode-hook
            (lambda ()
              (set (make-local-variable 'whitespace-style)
                   '(face trailing lines-tail))
              (whitespace-mode 1))))

(use-package flycheck
  :ensure t
  :bind (
         ("C-c C-x" . flycheck-next-error)
         ("C-c C-n" . flycheck-next-error) ; backup for cider
         )
  :config
  (setq flycheck-gcc-language-standard "c++2a")
  (setq flycheck-clang-language-standard "c++2a")
  :hook
  (after-init . global-flycheck-mode))

;;; lsp/eglot
(use-package eglot
  :ensure t
  :hook ((( clojure-mode clojurec-mode clojurescript-mode
            java-mode scala-mode rust-mode rust-ts-mode python-mode python-ts-mode)
          . eglot-ensure)
         ((cider-mode eglot-managed-mode) . eglot-disable-in-cider))
  :bind (:map eglot-mode-map
              ("C-c C-f" . eglot-format-buffer)
              ("C-c a" . eglot-code-actions)
              ("C-x C-u" . xref-find-references)
              ("C-x C-d" . xref-find-definitions)
              )
  :preface
  (defun eglot-disable-in-cider ()
    (when (eglot-managed-p)
      (if (bound-and-true-p cider-mode)
          (progn
            (remove-hook 'completion-at-point-functions 'eglot-completion-at-point t)
            (remove-hook 'xref-backend-functions 'eglot-xref-backend t))
        (add-hook 'completion-at-point-functions 'eglot-completion-at-point nil t)
        (add-hook 'xref-backend-functions 'eglot-xref-backend nil t))))
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  (eglot-extend-to-xref nil)
  (eglot-ignored-server-capabilities
   '(:documentHighlightProvider
     :colorProvider
     :foldingRangeProvider))
  (eglot-stay-out-of '(yasnippet)))


(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :init
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0)
  (setq company-require-match 'never)
  (setq-default company-dabbrev-other-buffers 'all
                company-tooltip-align-annotations t)
  (setq tab-always-indent 'complete)
  :bind
  ((:map company-mode-map
         ("M-'" . 'company-complete))
   (:map company-active-map
         ("C-n" . 'company-select-next)
         ("C-p" . 'company-select-previous))))

;; highlight parentheses
(use-package highlight-parentheses
  :ensure t
  :hook ((python-ts-mode
          emacs-lisp-mode
          rust-ts-mode
          clojure-mode
          text-mode) . highlight-parentheses-mode))


(use-package clojure-mode
  :defer t
  :commands put-clojure-indent
  :mode (("\\.boot\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode)
         ("\\.clj\\'" . clojure-mode)
         ("deps.edn" . clojure-mode))
  :init
  (setq clojure-indent-style :always-indent)
  (setq clojure-thread-all-but-last t)
  (setq gc-cons-threshold (* 100 1024 1024))
  (setq read-process-output-max (* 1024 1024))
  (setq company-minimum-prefix-length 1)
  (setq clojure-align-forms-automatically t)
  (setq clojure-toplevel-inside-comment-form t))

(use-package cider
  :defer t
  :no-require t
  :commands cider-mode
  :bind (:map cider-repl-mode-map
              ("C-c M-r" . cider-repl-previous-matching-input)
              ("C-c M-s" . cider-repl-next-matching-input))
  :hook
  ((clojure-mode . cider-mode)
   (cider-repl-mode . smartparens-mode))
  :config
  (setq
   cider-show-error-buffer nil
   cider-repl-history-file "~/.emacs.d/cider-history"
   cider-repl-display-help-banner nil))

;; (use-package clj-refactor
;;   :ensure t
;;   :hook ((clojure-mode . clj-refactor-mode))
;;   :init
;;   (cljr-add-keybindings-with-prefix "C-c C-m"))

(use-package smartparens
  :ensure t
  :commands smartparens-global-mode
  :hook
  ((lisp-mode . smartparens-strict-mode)
   (emacs-lisp-mode . smartparens-strict-mode)
   (clojure-mode . smartparens-strict-mode))
  :config
  (require 'smartparens-config)
  (show-smartparens-global-mode t)
  (show-paren-mode nil)
  :bind (:map smartparens-mode-map
              ("C-M-f" . sp-forward-sexp)
              ("C-M-b" . sp-backward-sexp)
              ("C-M-d" . sp-down-sexp)
              ("C-M-e" . sp-up-sexp)
              ("M-," . sp-beginning-of-sexp)
              ("M-." . sp-end-of-sexp)
              ("M-v" . sp-select-previous-thing)
              ("M-t" . sp-select-next-thing)
              ("C-M-n" . sp-next-sexp)
              ("C-M-p" . sp-previous-sexp)
              ("C-c (" . sp-backward-slurp-sexp)
              ("C-c )" . sp-forward-slurp-sexp)
              ("M-)" . sp-forward-barf-sexp)
              ("M-(" . sp-backward-barf-sexp)
              ("C-M-k" . sp-kill-sexp)
              ("C-k" . sp-kill-hybrid-sexp)
              ("C-M-w" . sp-copy-sexp)
              ("M-<delete>" . sp-unwrap-sexp)
              ("M-<backspace>" . sp-backward-unwrap-sexp)
              ("M-s" . sp-splice-sexp)
              ("M-c" . sp-copy-sexp)
              ("C-c c" . sp-comment)))

(use-package sql-indent
  :defer t
  :commands sqlind-minor-mode
  :hook ((sql-mode . sqlind-minor-mode)))


(use-package pyvenv-auto
  :defer t
  :hook (python-ts-mode . pyvenv-auto-run))

(use-package pyvenv
  :defer t
  :config
  (pyvenv-mode t))


(use-package python-ts-mode
  :mode "\\.py\\'"
  :defer t
  :hook ((python-ts-mode . smartparens-mode))
  :config
  (setq indent-tabs-mode nil)
  :init
  (setopt python-shell-completion-native-enable nil))


;; rust
(use-package rust-ts-mode
  :mode "\\.rs\\'"
  :defer t
  :bind (:map rust-ts-mode-map
              ("C-c C-b" . recompile)
              ("C-c C-c" . rust-run)
              )
  :hook ((rust-ts-mode . smartparens-mode)
         )
  :config
  (setq indent-tabs-mode nil)
  )

(use-package flycheck-eglot
  :after (eglot flycheck)
  :ensure t
  :init
  (global-flycheck-eglot-mode 1))

;; ocaml
(use-package tuareg
  :mode (("\\.ml[ily]?$" . tuareg-mode)
         ("\\.topml$" . tuareg-mode))
  :defer t
  :init
  (progn
    ;; Make OCaml-generated files invisible to filename completion
    (dolist (ext '(".cmo" ".cmx" ".cma" ".cmxa" ".cmi" ".cmxs" ".cmt" ".cmti" ".annot"))
      (add-to-list 'completion-ignored-extensions ext))))

(use-package utop
  :defer t
  :init
  (progn
    (add-hook 'tuareg-mode-hook 'utop-minor-mode))
  :config
  (progn
    (if (executable-find "opam")
        (setq utop-command "opam config exec -- utop -emacs")
      )))

(use-package merlin
  :defer t
  :hook ((tuareg-mode . merlin-mode))
  :init
  (setq merlin-completion-with-doc t))

;; Various
(use-package projectile
  :ensure t
  :commands projectile-mode
  :diminish projectile-mode
  :config (projectile-mode)
  :init
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'default)
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind (("C-x b" . projectile-switch-to-buffer)
         ("C-c i" . projectile-find-other-file)))

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
  :defer t
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
  :defer t
  :no-require t
  :commands dockerfile-mode
  :mode (("Dockerfile" . dockerfile-mode)))

(use-package lua-mode
  :defer t
  :mode (("\\.lua\\'" . lua-mode)))

(use-package q-mode
  :defer t
  :mode (("\\.q\\'" . q-mode)))

(use-package rainbow-delimiters
  :ensure t
  :commands rainbow-delimiters-mode-enable)

(use-package flyspell
  :ensure nil ;; otherwise checks every time?
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

(use-package kill-ring-search
  :ensure t)

(use-package telega
  :ensure t
  :defer t
  :bind-keymap ("C-c t" . telega-prefix-map)
  :commands (telega)
  :config
  (use-package telega-mnz)
  (defun my-telega-chat-mode ()
    (set (make-local-variable 'company-backends)
         (append (list 'telega-company-username
                       'telega-company-hashtag)
                 (when (telega-chat-bot-p telega-chatbuf--chat)
                   '(telega-company-botcmd))))
    (company-mode 1))
  (add-hook 'telega-chat-mode-hook 'my-telega-chat-mode)
  (add-hook 'telega-load-hook 'global-telega-squash-message-mode)
  (add-hook 'telega-load-hook 'global-telega-mnz-mode)

  (add-to-list 'telega-browse-url-alist '(".*" . browse-url-default-macosx-browser))

  (setq telega-emoji-use-images nil)

  (setq telega-webpage-preview-size-limits nil)
  (setq telega-webpage-preview-description-limit 64)
  (setq telega-open-file-function 'browse-url-default-macosx-browser)
  (setq telega-open-message-as-file '(video audio video-note voice-note))
  (setq telega-chat-send-disable-webpage-preview t)
  (setq telega-chat-fill-column 104)
  (setq telega-chat-folder-format "%I")
  (setq scroll-margin 3)
  (setq telega-root-default-view-function 'telega-view-two-lines)
  (setq telega-vvnote-video-cmd
        "ffmpeg -f avfoundation -s 640x480 -framerate 30 -i default -r 30 -f avfoundation -i :default -vf format=yuv420p,crop=240:240:240:40 -vcodec hevc -vb 300k -strict -2 -acodec opus -ac 1 -ab 32k"))
