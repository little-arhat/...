(defalias 'qrr 'query-replace-regexp)

(defun no-scroll-margin ()
  "Set scroll-margin to 0 buffer-locally"
  (interactive)
  (set (make-local-variable 'scroll-margin) 0))

(defun display-trailing-whitespace ()
  "Enable display of trailing whitespaces buffer-locally"
  (interactive)
  (set (make-local-variable 'show-trailing-whitespace) t))

(defun insert-date (format)
  "Wrapper around format-time-string."
  (interactive "MFormat: ")
  (insert (format-time-string format)))

(defun insert-standard-date ()
  "Inserts standard date time string."
  (interactive)
  (insert (format-time-string "%F %T")))

(defun whole-line ()
  "Returns list of two values - beginning of this line
and beginning of next line, for deleting/copying"
  (list (line-beginning-position) (line-beginning-position 2)))

(defun prh:copy-line ()
  "Save current line into Kill-Ring without marking the line "
  (buffer-substring (line-beginning-position) (line-end-position))
  )

(defun prh:check-newline (line)
  "Checks that line ends in newline. Adds it if not."
  (if (eql (aref line (1- (length line))) ?\n)
      line
    (concat line "\n")
  ))

(defun prh:cut-line ()
  "Kills current line"
  (let ((cutted-line (apply 'buffer-substring (whole-line))))
    (apply 'delete-region (whole-line))
    (prh:check-newline cutted-line)
    ))

(defun prh:count-lines (arg)
  "Count lines depending on arg.
If arg is positive, count from current position to end,
if negative, count from start to current position.
"
  (if (> arg 0)
      (count-lines (point) (point-max))
   (count-lines (point-min) (point))))

(defun prh:move-line (&optional arg)
  "Move current line.
Arg determines number of lines to skip, negative means move up."
  (interactive "p")
  (if (> (prh:count-lines arg) 0)
      (let ((prh:column (current-column)))
        (progn
          (or arg (setq arg 1))
          (forward-line 1)
          (transpose-lines arg)
          (forward-line -1)
          (move-to-column prh:column)))
    ))

(defun prh:move-line-down (&optional arg)
  "Move current line down. Optional ARG determines number of lines to skip"
  (interactive "p")
  (or arg (setq arg 1))
  (prh:move-line arg)
)

(defun prh:move-line-up (&optional arg)
  "Move current line up. Optional ARG determines number of lines to skip"
  (interactive "p")
  (or arg (setq arg 1))
  (prh:move-line (- arg))
)

(defun prh:duplicate-line (&optional arg)
  "Copy current line.
Arg determines number of lines to be created and direction."
  (interactive "p")
  (let ((cur-column (current-column))
        (cutted-line (prh:copy-line)))
    (progn
      (or arg (setq arg 1))
      (let ((tomove (if (< arg 0) (1+ arg) arg)))
        (end-of-line tomove))
      (newline)
      (insert cutted-line)
      (forward-line (- arg))
      (move-to-column cur-column)))
  )

(defun prh:duplicate-line-down (&optional arg)
  "Duplicate current line down. Optional ARG determines number of lines to skip"
  (interactive "p")
  (or arg (setq arg 1))
  (prh:duplicate-line arg)
)

(defun prh:duplicate-line-up (&optional arg)
  "Duplicate current line up. Optional ARG determines number of lines to skip"
  (interactive "p")
  (or arg (setq arg 1))
  (prh:duplicate-line (- arg))
)

(defun kill-region-or-word (&optional arg)
  "If region is active, kill it, backward kill word in other case."
  (interactive "p")
  (if (and transient-mark-mode mark-active)
      (kill-region (point) (mark))
    (backward-kill-word arg)
    ))

(defun toggle-file (desired-file)
  "Toggle buffer display of desired file."
  (when (file-exists-p desired-file)
    (if (and (buffer-file-name)
             (string= (expand-file-name desired-file)
                      (expand-file-name (buffer-file-name))))
        (bury-buffer)
      (find-file desired-file))))

(defun toggle-buffer (buffer-name)
  "Toggle display of desired buffer"
  (if (string= (buffer-name) buffer-name)
      (bury-buffer)
    (switch-to-buffer buffer-name t)))

(defun plist-maybe-get (keywords key)
  "If there is no \"key\" in \"keywords\", return nil.
Otherwise return value."
  (if (plist-member keywords key)
      (plist-get keywords key)
    nil))

(defun wc (region-min region-max)
  "Word count of whole buffer, if mark is active - of marked region"
  (interactive "r")
  (if (and transient-mark-mode mark-active)
      (message "Word count: %s" (how-many "\\w+" region-min region-max))
    (message "Word count: %s" (how-many "\\w+" (point-min) (point-max)))))

(defun sc (region-min region-max)
  "Symbol count of whole buffer, if mark is active - of marked region"
  (interactive "r")
  (if (and transient-mark-mode mark-active)
      (message "Symbol count: %s" (how-many "." region-min region-max))
    (message "Symbol count: %s" (how-many "." (point-min) (point-max)))))

(defalias 'cal 'calendar)

(defun beginning-of-line-dwim (arg)
  "Moves to beginning-of-line, or from there to the first non-whitespace character.

This takes a numeric prefix argument; when not 1, it behaves exactly like
\(move-beginning-of-line arg) instead."
  (interactive "p")
  (if (and (looking-at "^") (= arg 1)) (skip-chars-forward " \t") (move-beginning-of-line arg)))

(defvar selected-symbol)

(defun toggle-current-window-dedication ()
  (interactive)
  (let* ((window (selected-window))
         (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "")
             (buffer-name))))

(defun newline-maybe-indent ()
  "Like newline-and-indent, but doesn't indent if the previous line is blank"
  (interactive "*")
  (if (= (line-beginning-position) (line-end-position))
      (newline)
    (newline-and-indent)))

(defun djcb-snip (b e summ)
  "remove selected lines, and replace it with [snip:summary (n lines)]"
  (interactive "r\nsSummary:")
  (let ((n (count-lines b e)))
    (delete-region b e)
    (insert (format "[snip%s (%d line%s)]"
                    (if (= 0 (length summ)) "" (concat ": " summ))
                    n
                    (if (= 1 n) "" "s")))))

(defun time-to-number (time)
  "Convert time from format 9:30 to number"
  (let ((time (if (string-match ":" time)
                  (mapcar 'string-to-number (split-string time ":"))
                `(,(string-to-number time) 0))))
    (+ (car time) (/ (float (cadr time)) 60))))

(defun path-to-clipboard ()
  "Copy the current file's path to the clipboard.
  If the current buffer has no file, copy the buffer's default directory."
  (interactive)
  (let ((path (expand-file-name (or (buffer-file-name) default-directory))))
    (kill-new path)
    (message "%s" path)))

(defun ty:project-root-open-file (fname ext)
  (let* ((newnname (concat fname ext))
         (file-to-open (ty:project-root-find-file newnname)))
    (if file-to-open
        (find-file file-to-open)
      nil)))

(defun ty:project-root-find-file (filename)
  (with-project-root
      (let* ((project-files (project-root-files))
             (project-filenames (mapcar 'car project-files))
             (found-filename (ty:find-first
                              (lambda (s) (string-prefix-p filename s))
                              project-filenames)))
        (cdr (assoc found-filename project-files)))))

(defun ty:filter (condp lst)
  (delq nil (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun ty:find-first (condp lst)
  (car (ty:filter condp lst)))

(defun ty:project-root-find-tag (prompt)
  (interactive (find-tag-interactive "Find tags in project: "))
  (let ((project-tags-file-name (ty:project-root-find-file "TAGS")))
    (setq tags-file-name project-tags-file-name)
    (find-tag prompt)))


(defun insert-quotes ()
  "Inserts quotes (\") around the current region or work."
  (interactive)
  (let (start end bounds)
    (if (and transient-mark-mode mark-active)
        (setq start (region-beginning)
              end (region-end))
      (progn
        (setq bounds (bounds-of-thing-at-point 'symbol))
        (setq start (car bounds)
              end (cdr bounds))))
    (goto-char start)
    (insert "\"")
    (goto-char (+ end 1))
    (insert "\"")))

(defun my-pbcopy ()
  (interactive)
  (shell-command-on-region
   (region-beginning)
   (region-end) "pbcopy"))

(defun my-pbpaste ()
  (interactive)
  (shell-command "pbpaste" 1))

(defun search-from-clipboard ()
  "search for text from clipboard"
  (interactive)
  (deactivate-mark)
  (isearch-mode t nil nil nil)
  (isearch-yank-string (shell-command-to-string "pbpaste")))

(defun projectile-selection-at-point ()
  (when (use-region-p)
    (buffer-substring-no-properties (region-beginning) (region-end))))


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
