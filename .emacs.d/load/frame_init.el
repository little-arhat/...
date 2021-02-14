(setq server-socket-dir "~/.emacs.d/server")
;; (server-start)

(add-hook 'server-switch-hook
              (lambda nil
                (let ((server-buf (current-buffer)))
                  (bury-buffer)
                  (switch-to-buffer-other-frame server-buf))))
(add-hook 'server-done-hook 'delete-frame)
(add-hook 'server-done-hook (lambda nil (kill-buffer nil)))

(when graf
  ;; Title formatting
  (setq frame-title-format (list "emacs - "  '(buffer-file-name "%f" "%b")))
  (setq icon-title-format frame-title-format)

  ;; Font setup
  ;; nix font is configured in resources
  (if win
      (add-to-list 'default-frame-alist '(font . "-outline-Unifont-normal-r-normal-normal-16-120-96-96-c-*-*")))

  (tool-bar-mode 0)
)

(menu-bar-mode 0)


;; syntax highlight
(cond ((fboundp 'global-font-lock-mode)
      (global-font-lock-mode t)
;; Maximum colors
      (setq font-lock-maximum-decoration t)))

;;; themes
(if (display-graphic-p)
    (progn
      (load-theme 'solarized-light t)
      (setq solarized-termcolors 256)
      (setq solarized-bold nil)
      (setq solarized-use-less-bold t)
      (setq solarized-underline nil)
      (setq solarized-italic nil)
      (setq solarized-use-variable-pitch nil)
      (setq solarized-height-plus-1 1.0)
      (setq solarized-height-plus-2 1.0)
      (setq solarized-height-plus-3 1.0)
      (setq solarized-height-plus-4 1.0)
      (setq mac-command-modifier 'hyper)
      (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji"))
      (set-face-attribute 'default nil
                          :family "Fira Code"
                          :height 180
                          :weight 'normal
                          :width 'normal)))
