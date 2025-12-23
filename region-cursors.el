;;; region-cursors.el --- Display point and active mark using overlays -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Abidán Brito Clavijo

;; Author: Abidán Brito Clavijo <abidan.brito@gmail.com>
;; Version: 1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: convenience, editing, faces
;; URL: https://github.com/abidanBrito/region-cursors
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This package provides a minor mode that enhances cursor display when
;; a region is active.  When mark is set and region is active, both the
;; point and mark positions are highlighted with cursor-like overlays.
;; When no region is active, the standard cursor is used instead.

;;; Code:

(defgroup region-cursors nil
  "Display point and mark using cursor overlays."
  :group 'editing
  :group 'convenience)

(defcustom region-cursors-cursor-color "#ff6c6b"
  "Color used for both point and mark cursor overlays."
  :type 'color
  :group 'region-cursors)

(defcustom region-cursors-cursor-shape 'box
  "Shape of the region cursor overlay."
  :type '(choice (const :tag "box" box)
                 (const :tag "bar" bar))
  :group 'region-cursors)

(defcustom region-cursors-bar-width 2
  "Width of the bar cursor in pixels when `region-cursors-cursor-shape' is `bar`."
  :type 'integer
  :group 'region-cursors)

(defcustom region-cursors-rectangle-mode-support t
  "Whether to show corner cursors in `rectangle-mark-mode'."
  :type 'boolean
  :group 'region-cursors)

(defcustom region-cursors-blink nil
  "Whether cursor overlays should blink."
  :type 'boolean
  :group 'region-cursors)

(defcustom region-cursors-blink-interval 0.5
  "Blink interval in seconds when `region-cursors-blink' is non-nil."
  :type 'number
  :group 'region-cursors)

(defcustom region-cursors-disable-hl-line-mode nil
  "Whether to disable `hl-line-mode' while region is active."
  :type 'boolean
  :group 'region-cursors)

(defcustom region-cursors-disabled-modes '()
  "Major modes where `region-cursors' should not activate."
  :type '(repeat symbol)
  :group 'region-cursors)

(defvar-local region-cursors--point-overlay nil
  "Overlay for the point position.")

(defvar-local region-cursors--mark-overlay nil
  "Overlay for the mark position.")

(defvar-local region-cursors--rect-top-left-overlay nil
  "Overlay for rectangle top-left corner.")

(defvar-local region-cursors--rect-top-right-overlay nil
  "Overlay for rectangle top-right corner.")

(defvar-local region-cursors--rect-bottom-left-overlay nil
  "Overlay for rectangle bottom-left corner.")

(defvar-local region-cursors--rect-bottom-right-overlay nil
  "Overlay for rectangle bottom-right corner.")

(defvar-local region-cursors--region-was-active nil
  "Track if the region was active in the previous update.")

(defvar-local region-cursors--saved-cursor-type nil
  "Storage for the original cursor type.")

(defvar-local region-cursors--blink-timer nil
  "Timer for blinking cursor overlays.")

(defvar-local region-cursors--blink-state t
  "Current visibility state for blinking cursors (t = visible).")

(defvar-local region-cursors--hl-line-was-active nil
  "Track if `hl-line-mode' was active before region activation.")

(defun region-cursors--cursor-anchor (pos)
  "Return a safe overlay anchor position for POS, handling EOF."
  (if (and (= pos (point-max))
           (> pos (point-min)))
      (1- pos)
    pos))

(defun region-cursors--overlay-range (pos)
  "Return (START . END) range for a cursor overlay at POS."
  (pcase region-cursors-cursor-shape
    ('box
     (let* ((start (region-cursors--cursor-anchor pos))
            (end (min (1+ start) (point-max))))
       (cons start end)))
    ('bar
     ;; Zero-width overlay for bar cursor
     (cons pos pos))))

(defun region-cursors--hide-cursor ()
  "Hide the real cursor, saving its previous value."
  (unless region-cursors--saved-cursor-type
    (setq region-cursors--saved-cursor-type cursor-type)
    (setq cursor-type nil)
    (force-window-update (selected-window))))

(defun region-cursors--restore-cursor ()
  "Restore the real cursor if it was hidden."
  (when region-cursors--saved-cursor-type
    (setq cursor-type region-cursors--saved-cursor-type)
    (setq region-cursors--saved-cursor-type nil)
    (force-window-update (selected-window))))

(defun region-cursors--cancel-blink-timer ()
  "Cancel the blink timer if it exists."
  (when (timerp region-cursors--blink-timer)
    (cancel-timer region-cursors--blink-timer)
    (setq region-cursors--blink-timer nil)))

(defun region-cursors--toggle-blink ()
  "Toggle visibility of cursor overlays for blinking effect."
  (setq region-cursors--blink-state (not region-cursors--blink-state))
  (let ((color (if region-cursors--blink-state
                   region-cursors-cursor-color
                 "transparent")))
    (dolist (ov (list region-cursors--point-overlay
                      region-cursors--mark-overlay
                      region-cursors--rect-top-left-overlay
                      region-cursors--rect-top-right-overlay
                      region-cursors--rect-bottom-left-overlay
                      region-cursors--rect-bottom-right-overlay))
      (when (overlayp ov)
        (pcase region-cursors-cursor-shape
          ('box
           (overlay-put ov 'face `(:background ,color)))
          ('bar
           (overlay-put ov 'before-string
                        (propertize " "
                                    'display `(space :width (,region-cursors-bar-width))
                                    'face `(:background ,color)
                                    'cursor t))))))))

(defun region-cursors--start-blink-timer ()
  "Start the blink timer if blinking is enabled."
  (when region-cursors-blink
    (region-cursors--cancel-blink-timer)
    (setq region-cursors--blink-state t)
    (setq region-cursors--blink-timer
          (run-with-timer region-cursors-blink-interval
                          region-cursors-blink-interval
                          #'region-cursors--toggle-blink))))

(defun region-cursors--disable-hl-line ()
  "Disable `hl-line-mode' locally if requested."
  (when region-cursors-disable-hl-line-mode
    (cond
     ;; Local mode
     ((and (boundp 'hl-line-mode) hl-line-mode)
      (setq region-cursors--hl-line-was-active 'local)
      (hl-line-mode -1))
     ;; Global mode
     ((and (boundp 'global-hl-line-mode) global-hl-line-mode)
      (setq region-cursors--hl-line-was-active 'global)
      (global-hl-line-mode -1)))))

(defun region-cursors--restore-hl-line ()
  "Restore `hl-line-mode' if it was active before."
  (when (and region-cursors-disable-hl-line-mode
	     region-cursors--hl-line-was-active)
    (pcase region-cursors--hl-line-was-active
      ('local
       (hl-line-mode 1))
      ('global
       (global-hl-line-mode 1)))
    (setq region-cursors--hl-line-was-active nil)))


(defun region-cursors--cleanup-point-mark-overlays ()
  "Remove point and mark cursor overlays."
  (when (overlayp region-cursors--point-overlay)
    (delete-overlay region-cursors--point-overlay)
    (setq region-cursors--point-overlay nil))
  (when (overlayp region-cursors--mark-overlay)
    (delete-overlay region-cursors--mark-overlay)
    (setq region-cursors--mark-overlay nil)))

(defun region-cursors--cleanup-rectangle-overlays ()
  "Remove rectangle corner cursor overlays."
  (when (overlayp region-cursors--rect-top-left-overlay)
    (delete-overlay region-cursors--rect-top-left-overlay)
    (setq region-cursors--rect-top-left-overlay nil))
  (when (overlayp region-cursors--rect-top-right-overlay)
    (delete-overlay region-cursors--rect-top-right-overlay)
    (setq region-cursors--rect-top-right-overlay nil))
  (when (overlayp region-cursors--rect-bottom-left-overlay)
    (delete-overlay region-cursors--rect-bottom-left-overlay)
    (setq region-cursors--rect-bottom-left-overlay nil))
  (when (overlayp region-cursors--rect-bottom-right-overlay)
    (delete-overlay region-cursors--rect-bottom-right-overlay)
    (setq region-cursors--rect-bottom-right-overlay nil)))

(defun region-cursors--cleanup ()
  "Remove all region cursor overlays."
  (region-cursors--cancel-blink-timer)
  (region-cursors--cleanup-point-mark-overlays)
  (region-cursors--cleanup-rectangle-overlays))

(defun region-cursors--apply-shape (ov)
  "Apply cursor shape settings to overlay OV."
  (pcase region-cursors-cursor-shape
    ('box
     (overlay-put ov 'face `(:background ,region-cursors-cursor-color))
     (overlay-put ov 'before-string nil))
    ('bar
     (overlay-put ov 'face nil)
     (overlay-put ov 'before-string
                  (propertize " "
                              'display `(space :width (,region-cursors-bar-width))
                              'face `(:background ,region-cursors-cursor-color)
                              'cursor t)))))

(defun region-cursors--make-overlay (pos)
  "Create a cursor overlay at POS."
  (let* ((range (region-cursors--overlay-range pos))
         (ov (make-overlay (car range) (cdr range) nil t nil)))
    (overlay-put ov 'priority 1000)
    (region-cursors--apply-shape ov)
    ov))

(defun region-cursors--move-overlay (ov pos)
  "Move overlay OV to POS, handling EOF and shape."
  (let ((range (region-cursors--overlay-range pos)))
    (move-overlay ov (car range) (cdr range))
    (region-cursors--apply-shape ov)))

(defun region-cursors--rectangle-active-p ()
  "Return non-nil if `rectangle-mark-mode' is active."
  (and (boundp 'rectangle-mark-mode)
       rectangle-mark-mode
       region-cursors-rectangle-mode-support))

(defun region-cursors--get-rectangle-corners ()
  "Return rectangle corners as (top-left top-right bottom-left bottom-right).
Returns nil if rectangle is not valid (single line or column)."
  (when (region-cursors--rectangle-active-p)
    (let* ((start (region-beginning))
           (end (region-end))
           (start-col (save-excursion (goto-char start) (current-column)))
           (end-col (save-excursion (goto-char end) (current-column)))
           (start-line (line-number-at-pos start))
           (end-line (line-number-at-pos end))
           (left-col (min start-col end-col))
           (right-col (max start-col end-col))
           (top-line (min start-line end-line))
           (bottom-line (max start-line end-line)))
      ;; NOTE(abi): only show corners if we have an actual rectangle, not a line or a column.
      (when (and (> (abs (- right-col left-col)) 0)
                 (> (abs (- bottom-line top-line)) 0))
        (list
         ;; Top-left
         (save-excursion
           (goto-char (point-min))
           (forward-line (1- top-line))
           (move-to-column left-col)
           (point))

         ;; Top-right
         (save-excursion
           (goto-char (point-min))
           (forward-line (1- top-line))
           (move-to-column right-col)
           (point))

         ;; Bottom-left
         (save-excursion
           (goto-char (point-min))
           (forward-line (1- bottom-line))
           (move-to-column left-col)
           (point))

         ;; Bottom-right
         (save-excursion
           (goto-char (point-min))
           (forward-line (1- bottom-line))
           (move-to-column right-col)
           (point)))))))

(defun region-cursors--update ()
  "Update region cursor overlays."
  (unless (memq major-mode region-cursors-disabled-modes)
    (let ((region-active (use-region-p))
          (rect-active (region-cursors--rectangle-active-p)))
      (cond
       ;; Activation
       ((and region-active (not region-cursors--region-was-active))
	(setq region-cursors--region-was-active t)
	(region-cursors--hide-cursor)
	(region-cursors--disable-hl-line)
	(region-cursors--start-blink-timer))

       ;; Deactivation
       ((and (not region-active) region-cursors--region-was-active)
	(setq region-cursors--region-was-active nil)
	(region-cursors--cleanup)
	(region-cursors--restore-hl-line)
	(region-cursors--restore-cursor)))

      ;; Rendering
      (when region-active
	(region-cursors--hide-cursor)

	;; Rectangle mode
	(if rect-active
            (let ((corners (region-cursors--get-rectangle-corners)))
	      (when corners
		(unless (overlayp region-cursors--rect-top-left-overlay)
                  (setq region-cursors--rect-top-left-overlay
			(region-cursors--make-overlay (nth 0 corners))))
		(unless (overlayp region-cursors--rect-top-right-overlay)
                  (setq region-cursors--rect-top-right-overlay
			(region-cursors--make-overlay (nth 1 corners))))
		(unless (overlayp region-cursors--rect-bottom-left-overlay)
                  (setq region-cursors--rect-bottom-left-overlay
			(region-cursors--make-overlay (nth 2 corners))))
		(unless (overlayp region-cursors--rect-bottom-right-overlay)
                  (setq region-cursors--rect-bottom-right-overlay
			(region-cursors--make-overlay (nth 3 corners))))
		
		(region-cursors--move-overlay region-cursors--rect-top-left-overlay (nth 0 corners))
		(region-cursors--move-overlay region-cursors--rect-top-right-overlay (nth 1 corners))
		(region-cursors--move-overlay region-cursors--rect-bottom-left-overlay (nth 2 corners))
		(region-cursors--move-overlay region-cursors--rect-bottom-right-overlay (nth 3 corners)))

	      (region-cursors--cleanup-point-mark-overlays))
	  
          ;; Normal region mode
          (progn
            (unless (overlayp region-cursors--point-overlay)
	      (setq region-cursors--point-overlay
                    (region-cursors--make-overlay (point))))
            (unless (overlayp region-cursors--mark-overlay)
	      (setq region-cursors--mark-overlay
                    (region-cursors--make-overlay (mark))))
            (region-cursors--move-overlay region-cursors--point-overlay (point))
            (region-cursors--move-overlay region-cursors--mark-overlay (mark))

            (region-cursors--cleanup-rectangle-overlays)))))))

(defun region-cursors--post-command ()
  "Hook run after each command while `region-cursors-mode' is active."
  (region-cursors--update))

(defun region-cursors--window-change (_frame)
  "Handle selected window changing."
  (region-cursors--cleanup)
  (region-cursors--restore-cursor)
  (region-cursors--restore-hl-line)
  (setq region-cursors--region-was-active nil))

;;;###autoload
(define-minor-mode region-cursors-mode
  "Toggle `region-cursors-mode'.

  When enabled, cursor-like overlays replace the real cursor while
  a region is active."
  :global t
  :lighter " ●○"
  (if region-cursors-mode
      (progn
        (add-hook 'post-command-hook #'region-cursors--post-command)
        (add-hook 'window-selection-change-functions
                  #'region-cursors--window-change))
    (remove-hook 'post-command-hook #'region-cursors--post-command)
    (remove-hook 'window-selection-change-functions
                 #'region-cursors--window-change)
    (region-cursors--cleanup)
    (region-cursors--restore-cursor)
    (setq region-cursors--region-was-active nil)))

(provide 'region-cursors)

;;; region-cursors.el ends here
