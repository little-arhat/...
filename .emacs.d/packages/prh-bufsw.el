;;; prh-bufsw.el -- Support for a quick switch between Emacs buffers.

;; Author: Alexander Solovyov (Александр Соловьёв) <piranha@piranha.org.ua>
;; Version 1.4
;; Keywords: buffer
;; Based on pc-bufsw.el by Igor Bukanov
;; (http://www.mir2.org/igor/emacs/pc-bufsw.html)
;; and on code by Samuel Tesla
;; (http://www.emacswiki.org/cgi-bin/wiki/ControlTABbufferCycling#toc2)

;;  THIS SOFTWARE IS NOT COPYRIGHTED
;;
;;  This source code is offered for use in the public domain. You may
;;  use, modify or distribute it freely.
;;
;;  This code is distributed in the hope that it will be useful but
;;  WITHOUT ANY WARRANTY. ALL WARRANTIES, EXPRESS OR IMPLIED ARE HEREBY
;;  DISCLAMED. This includes but is not limited to warranties of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

;;; Commentary:

;; This is an implementation of quick switch for Emacs buffer that is similar
;; to one that is widely used on PC. The main idea here is that a user chooses
;; two keys (most often C-tab and S-C-tab) that switch between buffers
;; according to most or least recently order. After the final choice is made
;; the last selected buffer becomes most recently used one.
;; User can define it own buffers, that don't be used in switching.

;;; ChangeLog:

;; 1.4   2006-09-16
;; * Merged with Samuel Tesla functions

;;; Stesla helping functions

(defvar stesla-hated-buffers '("KILL" "*Apropos*" "*Completions*" "*grep*"
							   ".newsrc-dribble" ".bbdb" "sent-mail" "*vc*"
                               "*Compile-Log*" "*Help*" "*Messages*"))       

(defvar stesla-hated-buffer-regexps '("^ " "*Buffer" "^\\*trace" "^\\*tramp"))

(setq iswitchb-buffer-ignore (append stesla-hated-buffer-regexps  stesla-hated-buffers))

(defmacro stesla-buffer-regexp-mapcar (regexp buffers)
  "Find BUFFERS whose name matches REGEXP"
  `(mapcar (lambda (this-buffer)
             (if (string-match ,regexp (buffer-name this-buffer))
                 this-buffer))
           ,(if (symbolp buffers) (symbol-value buffers) buffers)))

(defmacro stesla-hated-buffer-from-regexps (regexps)
	"Generate a one-dimensional list of buffers that match REGEXPS"
  (append
   '(append)
   (mapcar (lambda (regexp)
             `(delete nil (stesla-buffer-regexp-mapcar ,regexp
                                                       (buffer-list))))
           (if (symbolp regexps) (symbol-value regexps) regexps))))

(defun stesla-delete-from-list (delete-these from-list)
  "Delete DELETE-THESE from FROM-LIST."
  (cond
   ((car delete-these)
    (if (member (car delete-these) from-list)
        (stesla-delete-from-list (cdr delete-these)
																 (delete (car delete-these) from-list))
      (stesla-delete-from-list (cdr delete-these) from-list)))
   (t from-list)))

(defun stesla-hated-buffers ()
  "List of buffers I never want to see."
  (delete nil
          (append
           (mapcar 'get-buffer stesla-hated-buffers)
           (stesla-hated-buffer-from-regexps stesla-hated-buffer-regexps))))


;;; Code:

(provide 'prh-bufsw)

(defvar pc-bufsw::quite-time 3
  "Quite time to automaticaly terminate buffer switch mode.
If there is no input during quite-time seconds makes the last choosen
buffer current." )

; Variable to store data vector during buffers change.
; Each element is (buffer .window) pair to show after i-th switch.
; The vector may contain any buffer refference several times if that was shown
; initially in several windows. It is supposed that buffers in the vector are
; odered according to most recently used oder.
(defvar pc-bufsw::walk-vector nil)

(defun pc-bufsw::get-buf (index)
  (car (aref pc-bufsw::walk-vector index)))

(defun pc-bufsw::get-wnd (index)
  (cdr (aref pc-bufsw::walk-vector index)))

; Index of currently selected buffer in pc-bufsw::walk-vector. Always even.
(defvar pc-bufsw::cur-index 0)

; The initial list of pairs (buffer display-time).
; When a user stops the selection, the new buffer order much the list
; except the selected buffer that is moved on the top. The time pairs allows
; to construct proper lru list and restore the initial access time for buffers.
; It is necessary since (buffer-list) does not follow lru order from user
; perspective due to frequent calls to (bury-buffer)
(defvar pc-bufsw::start-buf-list nil)

(defun pc-bufsw::previous ()
  (interactive)
  (pc-bufsw::walk 1))

(defun pc-bufsw::lru ()
  (interactive)
  (pc-bufsw::walk -1))

;;;###autoload
(defun pc-bufsw::bind-keys (key1 key2)
  "Bind key1 and key2 to switch buffers in most or least recently used oder.
Pressing key1 or key2 would switch to most or least recently used buffer and
enter switch mode. In this mode subsequent pressing of key1 or key2 would go
father in buffer list shown in echo area.

Pressing any other key or no input during the period indicated
by 'pc-bufsw::quite-time' variable closes the mode and makes the last selected buffer current.
If newly selected buffer is shown in some window that would be used to show
the buffer. Otherwise it will be displayed in the initial window.

Typical usage in .emacs file:
(require 'pc-bufsw)
(pc-bufsw::bind-keys [C-tab] [C-S-tab])
ot if you use XEmacs, do it like
(as suggested by Friedrich Dominicus <Friedrich.Dominicus@inka.de>)
(require 'pc-bufsw)
(pc-bufsw::bind-keys [(control tab)] [ (control shift tab) ])
"
  (global-set-key key1 'pc-bufsw::previous)
  (global-set-key key2 'pc-bufsw::lru))


; Main loop. It does 4 things. First, select new buffer and/or windows
; according to user input. Second, it selects the newly choosen buffer/windows/frame.
; Third, it draw in the echo area line with buffer names. Forth, it waits for a timeout
; to terminate the switching.
(defun pc-bufsw::walk (direction)
  (when (and (null pc-bufsw::walk-vector) (pc-bufsw::can-start))
    (setq pc-bufsw::start-buf-list (pc-bufsw::get-buffer-lru-list))
    ;;(setq pc-bufsw::start-buf-list (buffer-list))
    (setq pc-bufsw::cur-index 0)
    (setq pc-bufsw::walk-vector (pc-bufsw::get-walk-vector))
    (add-hook 'pre-command-hook 'pc-bufsw::switch-hook))
  (when pc-bufsw::walk-vector
    (pc-bufsw::choose-next-index direction)
    (pc-bufsw::choose-buf-and-wnd)
    (pc-bufsw::show-buffers-names)
    (when (sit-for pc-bufsw::quite-time)
      (pc-bufsw::finish))))

(defun pc-bufsw::can-start ()
  (not (window-minibuffer-p (selected-window))))

(defun pc-bufsw::get-buffer-display-time (buffer)
  (save-excursion
    (set-buffer buffer)
    buffer-display-time))

(defun pc-bufsw::set-buffer-display-time (buffer time)
  (save-excursion
    (set-buffer buffer)
    (setq buffer-display-time time)))

(defun pc-bufsw::get-buffer-lru-list()
  (let ((curtime (current-time))
	(curbuf (current-buffer)))
    (sort (mapcar (lambda (buf)
		    (cons buf (if (eq curbuf buf)
				  curtime
				(pc-bufsw::get-buffer-display-time buf))))
;		  (buffer-list))
		  (stesla-delete-from-list (stesla-hated-buffers) (buffer-list (selected-frame))))
	  (lambda (pair1 pair2)
	    "Retrn true if pair1 buffer was accessed later then pair2 buffer"
	    (let ((t1 (cdr pair1))
		  (t2 (cdr pair2)))
	      (cond
	       ((null t1) nil) ; null time can not be later then anything
	       ((null t2) t)   ; non-null is later then null
	       (t (>= (float-time t1) (float-time t2)))))))))

;; Hook to access next input from user.
(defun pc-bufsw::switch-hook ()
  (when (or (null pc-bufsw::walk-vector)
	    (not (or (eq 'pc-bufsw::lru this-command)
		     (eq 'pc-bufsw::previous this-command)
		     (eq 'handle-switch-frame this-command))))
    (pc-bufsw::finish)))

;; Construct main buffer/window vector.
(defun pc-bufsw::get-walk-vector()
  ; Assemble first buffer/window pairs into list and then make vector from
  ; its reverse. If buffer does not have window, the the current one is used.
  ; If buffer is shown in several windows, then they would be visited
  ; according to get-buffer-window-list results. This is a hack since in
  ; general window would not be returned in the order they were used, but
  ; Emacs has no API to do it properly.
  (let* ((wnd0 (selected-window))
	 (pair-list (list (cons (window-buffer wnd0) wnd0)))
	 (buf-list pc-bufsw::start-buf-list))
    (while buf-list
      (let ((buffer (car (car buf-list))))
	(when (pc-bufsw::can-work-buffer buffer)
	  (let ((wnd-list (get-buffer-window-list buffer 0 t)))
	    (if (null wnd-list)
		(setq pair-list (cons (cons buffer wnd0) pair-list))
	      (while wnd-list
		(let ((wnd (car wnd-list)))
		  (when (not (eq wnd wnd0))
		    (setq pair-list (cons (cons buffer wnd) pair-list))))
		(setq wnd-list (cdr wnd-list)))))))
      (setq buf-list (cdr buf-list)))
    (setq pair-list (nreverse pair-list))
    (apply 'vector pair-list)))

;;Return nill if buffer is not sutable for switch
(defun pc-bufsw::can-work-buffer (buffer)
  (let ((name (buffer-name buffer)))
    (not (char-equal ?\  (aref name 0)))))

;; Echo buffer list. Current buffer marked by <>.
(defun pc-bufsw::show-buffers-names()
  (let* ((width (frame-width))
	 (n (pc-bufsw::find-first-visible width))
	 (str (pc-bufsw::make-show-str n width)))
    (message "%s" str)))

(defun pc-bufsw::find-first-visible(width)
  (let ((first-visible 0)
	(i 1)
	(visible-length (pc-bufsw::show-name-len 0 t)))
    (while (<= i pc-bufsw::cur-index)
      (let ((cur-length (pc-bufsw::show-name-len i (= first-visible i))))
	(setq visible-length (+ visible-length cur-length))
	(when (> visible-length width)
	  (setq first-visible i)
	  (setq visible-length cur-length)))
      (setq i (1+ i)))
    first-visible))

(defun pc-bufsw::show-name-len(i at-left-edge)
  (+ (if at-left-edge 2 3)
     (length (buffer-name (pc-bufsw::get-buf i)))))

(defun pc-bufsw::make-show-str (first-visible width)
  (let* ((i (1+ first-visible))
	 (count (length pc-bufsw::walk-vector))
	 (str (pc-bufsw::show-name first-visible t))
	 (visible-length (length str))
	 (continue-loop (not (= i count))))
    (while continue-loop
      (let* ((name (pc-bufsw::show-name i nil))
	     (name-len (length name)))
	(setq visible-length (+ visible-length name-len))
	(if (> visible-length width)
	    (setq continue-loop nil)
	  (setq str (concat str name))
	  (setq i (1+ i))
	  (when (= i count)
	    (setq continue-loop nil)))))
    str))

(defun pc-bufsw::show-name(i at-left-edge)
  (let ((name (buffer-name (pc-bufsw::get-buf i))))
    (cond
     ((= i pc-bufsw::cur-index) (concat (if at-left-edge "<" " <") name ">"))
     (at-left-edge (concat " " name " "))
     (t (concat "  " name " ")))))

(defun pc-bufsw::choose-next-index (direction)
  (setq pc-bufsw::cur-index
	(mod (+ pc-bufsw::cur-index direction)
	     (length pc-bufsw::walk-vector))))

(defun pc-bufsw::ensure-window-selected (wnd)
  (when (not (eq wnd (selected-window)))
    (select-window wnd)
    (let ((frame (window-frame wnd)))
      (when (not (eq frame (selected-frame)))
	(if (not (fboundp `select-frame-set-input-focus))
	    (select-frame frame)
	  ;; TODO: it does not change active frame with some window managers
	  ;; under X11
	  (select-frame-set-input-focus frame))))))

(defun pc-bufsw::choose-buf-and-wnd ()
  (let ((buf (pc-bufsw::get-buf pc-bufsw::cur-index))
	(wnd (pc-bufsw::get-wnd pc-bufsw::cur-index))
	(wnd0 (pc-bufsw::get-wnd 0)))
    (if (eq wnd wnd0)
	;; The original buffer or buffer without window, show it in the
	;; initial window
	(progn
	  (pc-bufsw::ensure-window-selected wnd0)
	  (switch-to-buffer buf t))
      ;; Buffer originally shown in some window, switch to that window
      ;; but restore the initial buffer in the wnd0 first
      (let ((buf0 (pc-bufsw::get-buf 0)))
	(when (not (eq buf0 (window-buffer wnd0)))
	  (set-window-buffer wnd0 buf0)))
      (pc-bufsw::ensure-window-selected wnd))))

;; Called on switch mode close
(defun pc-bufsw::finish()
  (pc-bufsw::restore-order (aref pc-bufsw::walk-vector
				 pc-bufsw::cur-index)
			   pc-bufsw::start-buf-list)
  (remove-hook 'pre-command-hook 'pc-bufsw::switch-hook)
  (setq pc-bufsw::walk-vector nil)
  (setq pc-bufsw::cur-index 0)
  (setq pc-bufsw::start-buf-list nil)
  (message nil))

;; Put buffers in Emacs buffer list according to oder indicated by list
;; except put chosen-buffer to the first place.
(defun pc-bufsw::restore-order(chosen-buffer list)
  (while list
    (let ((buffer (car (car list)))
	  (time (cdr (car list))))
      (if (eq buffer chosen-buffer)
	  (pc-bufsw::set-buffer-display-time buffer (current-time))
	(bury-buffer buffer)
	(pc-bufsw::set-buffer-display-time buffer time)))
    (setq list (cdr list))))


