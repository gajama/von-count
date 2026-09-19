;; -*- lexical-binding: t; -*-

(defgroup von-count nil
  "Options for controlling the behaviour of Von Count mode."
  :group 'convenience :group 'von-count)

(defcustom von-count-target 750
  "The target number of words for Von Count mode."
  :type 'integer)

(defcustom von-count-bar-segment-char #x2588
  "The character that represents a ‘segment’ of the Von Count bar."
  :type 'character)

(defcustom von-count-done-color "green"
  "The Von Count bar ‘done’ color.

The color used to represent that portion of `von-count-total’ that has
been reached."
  :type 'color)

(defcustom von-count-todo-color "red"
  "The Von Count bar ‘todo’ color.

The color used to represent that portion of `von-count-total’ that has
not yet been reached."
  :type 'color)

(defface von-count-bar '((t . (:height 100)))
  "Face used for the Von Count bar.")

(defvar-local von-count-initial-word-count 0
  "The initial word-count.

The word-count of the buffer where Von Count mode is enabled.")

(defvar-local von-count-word-count-delta 0
  "The change in word count.

The difference between the buffer word-count now and
`von-count-initial-word-count'.")

(defvar-local von-count-last-word-count-delta 0)

(defvar-local von-count-bar-buffer nil
  "The buffer where the Von Count bar is displayed.

The bar buffer is displayed in an `atomic window’ below the buffer where
Von Count mode was enabled.")

(defvar-local von-count-parent-buffer nil
  "The buffer where Von Count mode was enabled.")

(defvar-local von-count-bar-length nil
  "The length in characters of the Von Count bar.

The Von Count bar resizes automatically when the window size changes.")

(defvar-local von-count-bar-last-length nil)

(defvar-local von-count-bar-done-pos 0
  "The done position on the Von Count bar.

The position along the bar in proportion to the amount of
`von-count-target’ that has been met.")

(defvar-local von-count-is-parent nil
  "Becomes true if Von Count mode is enabled in a buffer.")

(defvar-local von-count-is-bar nil
  "True in the Von Count bar buffer.")

(defun von-count-get-bar-window ()
  (if-let* ((bar-buffer-window
             (window-parameter (selected-window) 'von-count-bar-buffer-window)))
      (and (window-live-p bar-buffer-window)
    bar-buffer-window)))

(defun von-count-display-bar-buffer (&optional pos)
  "Initialize and display the Von Count buffer and bar.

If POS is passed, this specifies the position from which counting of
'new words' should begin The buffer is displayed in an `atomic window’
below the window containing the buffer where Von Count mode was
enabled. The bar is resized automatically if the window width changes."
  (interactive)
  (make-local-variable 'von-count-target)
  (let* ((bar-buffer-window (von-count-get-bar-window))
         (bar-buffer-name (concat "von Count " (buffer-name))))
    (message "Von Count bar buffer window: %s" bar-buffer-window)
    (setq-local von-count-bar-buffer
                (get-buffer-create bar-buffer-name)
                von-count-is-parent t
                von-count-word-count-delta 0
                von-count-last-word-count-delta 0)
    (von-count-store-file-word-count (or pos))
    (if bar-buffer-window
        (set-window-buffer bar-buffer-window von-count-bar-buffer)
      (let ((display-buffer-alist
             `((t
                (display-buffer-in-atom-window)
                (window . ,(selected-window))
                (window-height . (body-lines . 2))
                (preserve-size . (nil . t))
                (reusable-frames . visible)
                (window-parameters ((no-other-window . t)
                                    (delete-window . t)
                                    (no-delete-other-window . t)))))))
        (display-buffer von-count-bar-buffer)))
    (set-window-parameter (selected-window) 'von-count-bar-buffer-window (get-buffer-window von-count-bar-buffer))
    (message "Window: %s %s %s" von-count-bar-buffer (get-buffer-window von-count-bar-buffer) (window-parameter (selected-window) 'von-count-bar-buffer-window))
    (let ((parent-buffer (current-buffer)))
      (with-current-buffer von-count-bar-buffer
        (setq-local von-count-parent-buffer parent-buffer 
                    von-count-is-bar t)
        (von-count-bar-buffer-mode-line)
        (setq cursor-type nil)
        (setq buffer-read-only t)
        (add-hook 'window-size-change-functions #'von-count-bar-maybe-redraw 0 t)))
    (von-count-bar-buffer-setup (von-count-bar-get-length))
    (add-hook 'after-change-functions #'von-count-after-change-function 0 t)
    (add-hook 'kill-buffer-hook #'von-count-killed-remove-bar-buffer 0 t)))

(defun von-count-restart-count-from-point()
  "Restart von-count with the initial word count set to those in the region
between point-min and point."
  (interactive)
  (von-count-display-bar-buffer (point)))

(defun von-count-remove-bar-buffer()
  "Remove Von Count bar and clean up."
  (interactive)
  (remove-hook 'after-change-functions #'von-count-after-change-function t)
  (remove-hook 'kill-buffer-hook #'von-count-killed-remove-bar-buffer t)
  (when (and von-count-bar-buffer (buffer-live-p von-count-bar-buffer))
    (progn
      (with-current-buffer von-count-bar-buffer
        (walk-window-subtree #'(lambda(window)
                                 (set-window-parameter window 'window-atom nil))
                             (window-atom-root) 'any)
        (kill-buffer))))
  (setq-local von-count-bar-buffer nil
              von-count-is-parent nil))

(defun von-count-killed-remove-bar-buffer ()
  "If this buffer is parent of a Von Count bar buffer, remove the bar
buffer if this buffer is killed."
  (and von-count-is-parent
    (von-count-remove-bar-buffer)))

(defun von-count-refresh-bar-buffer-window ()
  (interactive)
  (if-let* ((bar-window (window-parameter (selected-window) 'von-count-bar-buffer-window))
            (bar-buffer von-count-bar-buffer))
      (set-window-buffer bar-window bar-buffer)))

(remove-hook 'buffer-list-update-hook #'von-count-refresh-bar-buffer-window t)
(add-hook 'buffer-list-update-hook #'von-count-refresh-bar-buffer-window 0 t)

(defmacro von-count-wrapper (parent-or-bar &rest body)
  "Wrapper that makes sure BODY is called in the correct buffer.

PARENT-OR-BAR should be one of either `'parent' or `bar', and specifies
in which buffer BODY is called."
  (let* ((sym (symbol-name parent-or-bar))
         (which (intern-soft (concat "von-count-is-" sym)))
         (buf (intern-soft (concat "von-count-" sym "-buffer"))))
    
  `(with-current-buffer (or (and ,which (current-buffer)) ,buf)
     ,@body)))

(defmacro von-count-with-bar-buffer (&rest body)
  `(von-count-wrapper bar ,@body))

(defmacro von-count-with-parent-buffer (&rest body)
  `(von-count-wrapper parent ,@body))

(defun von-count-bar-buffer-setup (bar-length)
  "Create a bar of BAR-LENGTH copies of `von-count-segment-char' in `von-count-bar-buffer'."
  (let ((inhibit-modification-hooks 'inhibit)
        (done-pos (von-count-bar-get-done-pos))
        (bar (propertize (make-string bar-length von-count-bar-segment-char) 'face 'von-count-bar)))
    (von-count-with-bar-buffer
     (von-count-bar--set-colors bar-length done-pos bar)
     (let ((inhibit-read-only t))
       (erase-buffer)
       (insert bar)))))

(defun von-count-bar-set-colors()
  "Set the colors of the Von Count bar.

The bar’s colors represent the portion of `von-count-target’ typed so
far.  The ‘done’ portion is coloured with `von-count-done-color’, and the
‘todo’ portion with `von-count-todo-color’.  Each segment of the bar is
the character `von-count-segment-char’.  The bar has an associated face:
`von-count-bar’."
  (von-count-with-bar-buffer
   (von-count-bar--set-colors
    (von-count-bar-get-length)
    (von-count-bar-get-done-pos))))

(defun von-count-bar--set-colors(bar-length done-pos &optional bar)
  "Internal function to set the Von Count bar colors.

BAR-LENGTH is the length of the Von Count bar. DONE-POS is the character
position along the Von Count bar representing the proportion of
`von-count-target’ words that have been typed so far. By default, the
bar in `von-count-bar-buffer’ is the target of this function; the
optional BAR can be used to pass in a string object instead. If BAR is
used its length should be the same as the value of BAR-LENGTH."
  (let*
      ((start (or (and bar 0) 1))
       (bar-length (+ start bar-length))
       (done-pos (min (+ start done-pos) bar-length))
       (inhibit-read-only t))
     (set-text-properties done-pos  bar-length
                         `(face (von-count-bar (:foreground ,von-count-todo-color))) bar)
    (and (> done-pos 0)
         (set-text-properties start done-pos
                              `(face (von-count-bar (:foreground ,von-count-done-color))) bar))))

(defun von-count-after-change-function (beg end len)
  "if words have been added to the `von-count-parent-buffer', update the
bar."
  (and (not (= (von-count-get-delta) von-count-last-word-count-delta))
       (setq von-count-last-word-count-delta von-count-word-count-delta)
       (von-count-bar-buffer-mode-line)
       (von-count-bar-set-colors)))

(defun von-count-bar-maybe-redraw (&optional frame)
  "Redraw the Von Count bar if needed.

If the width of the window where `von-count-bar-buffer’ is displayed has
changed, redraw the bar."
  (von-count-with-bar-buffer
   (and (not (=
              (von-count-bar-get-length)
              (or von-count-bar-last-length
                  (setq-local von-count-bar-last-length 0))))
        (setq-local von-count-bar-last-length von-count-bar-length)
        (von-count-bar-buffer-setup von-count-bar-length))))

(defun von-count-bar-buffer-p ()
   (and von-count-is-parent von-count-bar-buffer))

(defun von-count-bar-get-done-pos ()
  "Calculate ‘done’ position on a Von Count bar.

This is the character position that corresponds to the proportion of
 `von-count-target’ words typed so far."
  (von-count-with-bar-buffer
   (let* ((worked  (/ (* 1000 (von-count-get-target)) (von-count-bar-get-length))))
     (setq-local von-count-bar-done-pos (/ (* 1000 (von-count-get-delta)) worked)))))

(defun von-count-bar-get-done-pos ()
  "Calculate ‘done’ position on a Von Count bar.

This is the character position that corresponds to the proportion of
 `von-count-target’ words typed so far."
  (von-count-with-bar-buffer
   (let* ((worked  (/ (* 1000 (von-count-get-target)) (von-count-bar-get-length))))
     (setq-local von-count-bar-done-pos (/ (* 1000 (von-count-get-delta)) worked)))))

(defun von-count--get-word-count(&optional pos)
  (von-count-with-parent-buffer
   (count-words (point-min) (or pos (point-max)))))

(defun von-count-store-file-word-count(&optional pos)
  "Store the initial word count of the file visited in the buffer.

The optional POS specifies the point at which counting should stop. By
default the word count is of the whole buffer"
  (von-count-with-parent-buffer
   (setq-local von-count-initial-word-count (von-count--get-word-count pos))))

(defun von-count-bar-get-length()
  "Return the length of the bar."
  (von-count-with-bar-buffer
   (setq-local von-count-bar-length
               (window-max-chars-per-line
                (get-buffer-window (current-buffer) t) 'von-count-bar))
   von-count-bar-length))

(defun von-count-get-delta()
  "Return the difference between the word count now, and the previous word count.

If the difference is negative, return 0."
  (von-count-with-parent-buffer
   (and (< (setq-local von-count-word-count-delta
                       (- (von-count--get-word-count) von-count-initial-word-count)) 0)
        ;; If we've shrunk the file to fewer words than it initally had,
        ;; reset word-count state
        (setq-local von-count-word-count-delta 0
                    von-count-last-word-count-delta 0)
        (von-count-store-file-word-count))
   von-count-word-count-delta))

(defun von-count-get-target()
  (von-count-with-parent-buffer
  von-count-target))

(defun von-count-bar-buffer-mode-line()
  "Set a simple mode-line for the Von Count bar buffer."
  (von-count-with-bar-buffer
  (setq mode-line-format
          (format-mode-line
           '((:eval (format "%s  %s/%s"
                            (buffer-name von-count-bar-buffer)
                            (von-count-get-delta)
                            (von-count-get-target))))))
    (force-mode-line-update))
  t)

(defun von-count-mode-toggle ()
  (interactive)
  (if von-count-mode
       (von-count-display-bar-buffer)
    (von-count-remove-bar-buffer)))

;;;###autoload
(define-minor-mode von-count-mode
  "Toggle Von Count mode.

Von Count mode provides visual feedback on how many words have been
typed in a buffer. It shows a bar that indicates how many words of a
specified target number have been entered. The buffer that Von Count
mode watches is the buffer that is current when Von Count mode is
enabled."
  :lighter nil
  (von-count-mode-toggle))

(provide 'von-count)

;;; von-count.el ends here
