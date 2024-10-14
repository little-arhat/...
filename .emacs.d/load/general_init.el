;; general customizations

(defconst win
  (eq system-type 'windows-nt)
  "Are we running on Win32 system")

(defconst nix
  (not (eq system-type 'windows-nt))
  "Are we running on *nix system")

(defconst graf
  (not (eq window-system 'nil))
  "Are we running window system?")

;; don't ask, just do it!
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'overwrite-mode 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(setq
 inhibit-startup-message t           ;; don't show annoing startup msg
 native-comp-async-report-warnings-errors nil
 warning-minimum-level :error
 make-backup-files nil               ;; NO annoing backups
 vc-follow-symlinks t                ;; follow symlinks and don't ask
 echo-keystrokes 0.01                ;; see what you type
 scroll-conservatively 15            ;; text scrolling
 scroll-preserve-screen-position t
 scroll-margin 6
 scroll-step 1                       ;; Scroll by one line at a time
 comint-completion-addsuffix t       ;; Insert space/slash after completion
 kill-whole-line t                   ;; delete line in one stage
 major-mode 'text-mode       ;; default mode
 delete-key-deletes-forward t        ;; meaning are the same as the name :)
 next-line-add-newlines nil          ;; don't add new lines when scrolling down
 require-final-newline t             ;; make sure file ends with NEWLINE
 mode-require-final-newline t        ;; same as above, set more generally
 delete-old-versions t               ;; delete excess backup versions
 tab-width 4
 mouse-yank-at-point t               ;; paste at cursor, NOT at mouse pointer position
 apropos-do-all t                    ;; apropos works better but slower
 display-time-24hr-format t
 display-time-day-and-date t
 calendar-date-style 'european
 calendar-week-start-day 1
 auto-save-interval 512              ;; autosave every 512 keyboard inputs
 auto-save-list-file-prefix nil
 cursor-in-non-selected-windows nil
 safe-local-variable-values '((encoding . utf-8) (prompt-to-byte-compile))
 dabbrev-case-fold-search nil        ;; Case is significant for dabbrev
 split-width-threshold 110
 kill-ring-max 2000                  ;; oh yes! long killring!
 vc-handled-backends nil
 )

(set-variable 'default-truncate-lines t)
(set-default 'truncate-lines t)


(setq-default
 save-place t         ;; save position in files
 case-fold-search t   ;; case INsensitive search
 indent-tabs-mode nil ;; do not use tabs for indentation
 fill-column 80       ;; number of chars in line
)

;; Ask questions with short answers
(fset 'yes-or-no-p 'y-or-n-p)

(prefer-coding-system 'utf-8-unix)

(setq ring-bell-function (lambda () ())) ;; I hate beeps

(if (eq system-type 'darwin)
    (setq ns-extended-platform-support-mode t
          ns-command-modifier 'meta))

;; from https://github.com/purcell/exec-path-from-shell/blob/master/exec-path-from-shell.el
(defun exec-path-from-shell-setenv (value)
  "Set the value of environment var PATH to VALUE.
Also update the variables `exec-path' and `eshell-path-env'."
  (setenv "PATH" value)
  (setq exec-path (append (parse-colon-path value) (list exec-directory)))
  (setq-default eshell-path-env value))
(if (display-graphic-p)
    (exec-path-from-shell-setenv (shell-command-to-string "zsh -c 'echo $PATH'")))

(setq read-extended-command-predicate #'command-completion-default-include-p)
