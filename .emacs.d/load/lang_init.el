(set-selection-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; This works for copying, but not pasting for some reason
(setq select-enable-clipboard t)

;; Whatever... it's easy enough to implement that part ourselves
(setq interprogram-paste-function
      (lambda ()
        (shell-command-to-string "pbpaste")))

;; I like having unix lineendings even on windows
(prefer-coding-system 'utf-8-unix)
