;;; package -- summary

;;;;; completion & navigation
(use-package vertico
  :init
  (vertico-mode)
  ;; Different scroll margin
  (setq vertico-scroll-margin 0)
  ;; number of candidates
  (setq vertico-count 12)
  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize nil)
  ;; enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t))

(use-package savehist
  :init
  (savehist-mode)
  (setq savehist-additional-variables
        '(search-ring
          regexp-search-ring
          kill-ring
          file-name-history
          command-history
          shell-command-history)))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :bind
  ("C-x C-b" . consult-buffer)
  ("C-x b" . consult-project-buffer)
  ("C-x c p" . consult-ripgrep)
  ("C-x c r" . consult-register)
  ("C-x c d" . consult-register-load)
  ("C-x c y" . consult-yank-from-kill-ring)
  ("C-x c l" . consult-line)
  ("C-x c m" . consult-flymake) ;; overwrite mail
  ("C-x c f" . consult-fd) ;; overwrite set-fill-column
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  ;; The :init configuration is always executed (Not lazy)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  ;; configure preview
  (setq consult-preview-key "M-.") ;; 'any is for any key
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root))))


;;;;; visual
(use-package diminish :ensure t)

(use-package whitespace
  :diminish global-whitespace-mode
  :config
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
  ;; magit-mode.
  (defun galactic-emacs-prevent-whitespace-mode-for-magit ()
    (not (derived-mode-p 'magit-mode)))
  (add-function :before-while whitespace-enable-predicate 'galactic-emacs-prevent-whitespace-mode-for-magit)
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face trailing tabs lines-tail))
  (global-whitespace-mode t))


;;;;; flymake, spell, lsp, etc.
(use-package eglot
  :ensure t
  :hook ((( clojure-mode
            clojurec-mode
            clojurescript-mode
            java-mode scala-mode rust-mode
            rust-ts-mode
            python-mode
            python-ts-mode
            c-mode
            c-ts-mode
            c++-ts-mode)
          . eglot-ensure)
         ((cider-mode eglot-managed-mode) . eglot-disable-in-cider)
         (eglot-managed-mode . (lambda () (eglot-inlay-hints-mode -1)))
         )
  :bind (:map eglot-mode-map
              ("C-c C-f" . eglot-format-buffer)
              ("C-c a" . eglot-code-actions)
              ("C-x C-u" . xref-find-references)
              ("C-x C-d" . xref-find-definitions)
              )
  :config
  (add-to-list 'eglot-server-programs '((c++-ts-mode c-ts-mode) "clangd"))
  (setq eglot-inlay-hints-mode nil)
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
  (eldoc-echo-area-use-multiline-p nil)
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  (eglot-extend-to-xref nil)
  (eglot-ignored-server-capabilities
   '(:documentHighlightProvider
     :colorProvider
     :foldingRangeProvider))
  (eglot-stay-out-of '(yasnippet)))

(use-package flymake
  :custom (flymake-mode-line-lighter "ƒ")
  :bind (:map flymake-mode-map
              ("C-c C-x" . flymake-goto-next-error)
              ("C-c C-p" . flymake-goto-prev-error))
  :config
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  :hook ((prog-mode . flymake-mode)
         (text-mode . flymake-mode)))

(use-package flymake-aspell
  :ensure nil ;; otherwise checks every time?
  :custom
  (ispell-program-name "aspell")
  (ispell-dictionary "en_GB")
  (ispell-silently-savep t)
  :hook
  ((text-mode . flymake-aspell-setup)))


;;;;; project, vcs, etc
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

(use-package projectile
  :ensure t
  :commands projectile-mode
  :diminish projectile-mode
  :config (projectile-mode)
  :init
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'default)
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind (("C-c i" . projectile-find-other-file)))


;;;;; languages
;; parens
(use-package highlight-parentheses
  :ensure t
  :hook ((python-ts-mode
          emacs-lisp-mode
          rust-ts-mode
          clojure-mode
          text-mode) . highlight-parentheses-mode))

(use-package rainbow-delimiters
  :ensure t
  :commands rainbow-delimiters-mode-enable)

(use-package smartparens
  :ensure t
  :commands smartparens-global-mode
  :hook
  ((lisp-mode . smartparens-strict-mode)
   (emacs-lisp-mode . smartparens-strict-mode)
   (clojure-mode . smartparens-strict-mode)
   (clojure-ts-mode . smartparens-strict-mode))
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

;; clojure
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

;; rust
(use-package rust-ts-mode
  :mode "\\.rs\\'"
  :defer t
  :bind (:map rust-ts-mode-map
              ("C-c C-b" . recompile)
              ("C-c C-c" . rust-run)
              )
  :hook ((rust-ts-mode . smartparens-mode))
  :config
  (setq indent-tabs-mode nil))

;; python
(use-package pyvenv
  :defer t
  :config
  (pyvenv-mode t))

(use-package pyvenv-auto
  :defer t
  :hook (python-ts-mode . pyvenv-auto-run))

(use-package python-ts-mode
  :mode "\\.py\\'"
  :defer t
  :hook ((python-ts-mode . smartparens-mode))
  :config
  (setq indent-tabs-mode nil)
  :init
  (setopt python-shell-completion-native-enable nil))

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
        (setq utop-command "opam config exec -- utop -emacs"))))

(use-package merlin
  :defer t
  :hook ((tuareg-mode . merlin-mode))
  :init
  (setq merlin-completion-with-doc t))

;; cpp
(use-package c-ts-mode
  :ensure nil
  :config
  (setq c-ts-mode-indent-offset tab-width)
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist
               '(c-or-c++-mode . c-or-c++-ts-mode)))

;; https://github.com/uyha/tree-sitter-cmake
(use-package cmake-ts-mode
  :ensure nil)

(use-package cmake-ide
  :defer t
  :bind (("C-c C-b" . cmake-ide-compile)
         ("C-c C-r" . cmake-ide-run-cmake))
  :init
  (use-package projectile :ensure t)
  (defun my-cmake-ide-setup ()
    (when (projectile-project-p)
      (put 'cmake-ide-project-dir 'safe-local-variable 'stringp)
      (put 'cmake-ide-build-dir 'safe-local-variable 'stringp)
      (setq cmake-ide-project-dir (projectile-project-root))
      (setq cmake-ide-build-dir (concat cmake-ide-project-dir "build")))
    (setq cmake-ide-cmake-args '("-G" "Ninja"  "-DCMAKE_BUILD_TYPE=Release"))
    (cmake-ide-setup))
  (add-hook 'cmake-ts-mode-hook 'my-cmake-ide-setup)
  (add-hook 'c-ts-mode-hook 'my-cmake-ide-setup)
  (add-hook 'c++-ts-mode-hook 'my-cmake-ide-setup))

(use-package clang-format
    :config
    (defun my-c-cpp-mode-clang-format-on-save ()
      (add-hook 'before-save-hook 'clang-format-buffer nil t))
    (add-hook 'c-ts-mode-hook 'my-c-cpp-mode-clang-format-on-save)
    (add-hook 'c++-ts-mode-hook 'my-c-cpp-mode-clang-format-on-save)
  :ensure t)

;; other

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

(use-package markdown-mode
  :defer t
  :mode ("\\.markdown\\'" . markdown-mode)
        ("\\.md\\'" . markdown-mode)
  :bind (:map markdown-mode-map
         ("M-<right>" . nil)
         ("M-<left>" . nil)))

(use-package sql-indent
  :defer t
  :commands sqlind-minor-mode
  :hook ((sql-mode . sqlind-minor-mode)))

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

;;;;; misc

(use-package org
  :defer t
  :bind (:map org-mode-map
              ("C-," . nil)
              ("C-S-<up>" . org-timestamp-up)
              ("C-S-<down>" . org-timestamp-down))
  :config
  (setq org-hide-leading-stars t))


;;;;; Various

(use-package string-inflection
  :ensure t)

(use-package solarized-theme
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
    (global-whitespace-mode -1)
    (company-mode 1))
  (add-hook 'telega-chat-mode-hook 'my-telega-chat-mode)
  (add-hook 'telega-load-hook 'global-telega-squash-message-mode)
  (add-hook 'telega-load-hook 'global-telega-mnz-mode)
  (add-hook 'telega-chat-mode-hook (lambda () (global-whitespace-mode -1)))

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

;;;;
(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))


;;;; GPT/ai
(use-package copilot
  :custom
  (copilot-indent-offset-warning-disable t)
  :bind
  (("C-x A c m" . copilot-mode))
  :bind (:map copilot-mode-map
              ("M-RET" . copilot-accept-completion)
              ("M-n" . copilot-next-completion)
              ("M-p" . copilot-previous-completion)
              ("C-x A c c" . copilot-clear-overlay)))

(use-package copilot-chat
  :ensure t
  :bind (("C-x A c x" . copilot-chat-display)))

(use-package gptel
  :bind (("C-h C-t" . gptel-send)  ;; C-u C-h RET for gptel-menu
         ("C-h C-g" . gptel))
  :config
  (setq gptel-directives
        '((default . "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")
          (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
          (writing . "You are a large language model and a writing assistant. Respond concisely.")
          (chat . "You are a large language model and a conversation partner. Respond concisely.")
          (bug . "You are a large language model and a careful programmer. The supplied code doesn't work, or contains bugs. Describe each problem using only one sentence. Provide fixes without changing the old behavior.")))

  (defun openai-api-key ()
    (require 'gptel)
    (gptel-api-key-from-auth-source "api.openai.com"))

  (defun deepseek-api-key ()
    (require 'gptel)
    (gptel-api-key-from-auth-source "api.deepseek.com"))

  (defun grok-api-key ()
    (require 'gptel)
    (gptel-api-key-from-auth-source "api.x.ai"))

  ;; DeepSeek
  (gptel-make-deepseek "DeepSeek"
    :stream t
    :key #'deepseek-api-key)

  ;; Anthropic
  (gptel-make-xai "Grok"
    :stream t
    :key #'grok-api-key)

  (gptel-make-tool
   :name "create_python_repl"
   :function (lambda ()
               (run-python nil t)
               (pop-to-buffer (python-shell-get-buffer)))
   :description "Create a new python repl for this session"
   :args nil
   :category "emacs")

  (gptel-make-tool
   :name "send_python_to_repl"
   :function (lambda (code)
               (python-shell-send-string code))
   :args (list '(:name "code"
                       :type string
                       :description "python code to execute"))
   :description "Send some python code to the python repl for this session and execute it"
   :category "emacs")

  (gptel-make-tool
   :category "web"
   :name "search"
   :async t
   :function (lambda (cb keyword)
               (tavily-search-async cb keyword))
   :description "Search the Internet; If you used any search results, be sure to include the references in your response."
   :args (list '(:name "keyword"
                       :type string
                       :description "The keyword to search")))

  (gptel-make-tool
   :category "web"
   :name "fetch_url_text"
   :async t
   :description "Fetch the Markdown contents from an HTML page specified by its URL"
   :args (list '(:name "url"
                       :type string
                       :description "The url of the web page"))
   :function (lambda (cb url)
               (fetch-url-text-async cb url))))
