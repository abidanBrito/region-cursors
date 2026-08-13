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

(defcustom region-cursors-point-cursor-color nil
  "Color used for the point cursor overlay.
When non-nil, it overrides the `region-cursors-point-face' face."
  :type '(choice (const :tag "Use `region-cursors-point-face' face" nil)
                 color)
  :group 'region-cursors)

(defcustom region-cursors-mark-cursor-color nil
  "Color for the mark cursor overlay.
When non-nil, overrides the `region-cursors-mark-face' face."
  :type '(choice (const :tag "Use `region-cursors-mark-face' face" nil)
                 color)
  :group 'region-cursors)

(defcustom region-cursors-rectangle-cursor-color nil
  "Color for rectangle corner cursor overlays.
When non-nil, overrides the `region-cursors-rectangle-face' face."
  :type '(choice (const :tag "Use `region-cursors-rectangle-face' face" nil)
                 color)
  :group 'region-cursors)

(defcustom region-cursors-cursor-shape 'box
  "Shape of the region cursor overlay."
  :type '(choice (const :tag "box" box)
                 (const :tag "bar" bar))
  :group 'region-cursors)

(defcustom region-cursors-bar-width 2
  "Width of the bar cursor in pixels when `region-cursors-cursor-shape' is `bar`.
The bar is drawn as an inserted space, so it shifts the following text by this
many pixels as the cursor moves.  A smaller value reduces that shift; while the
`box' shape avoids it entirely by recoloring the existing glyph."
  :type 'integer
  :safe #'integerp
  :group 'region-cursors)

(defcustom region-cursors-rectangle-mode-support nil
  "Whether to show corner cursors in `rectangle-mark-mode'."
  :type 'boolean
  :safe #'booleanp
  :group 'region-cursors)

(defcustom region-cursors-animation 'pulse
  "Animation style for cursor overlays.
- `none': no animation.
- `blink': toggle visibility on and off.
- `pulse': fade in and out smoothly."
  :type '(choice (const :tag "None" none)
                 (const :tag "Blink" blink)
                 (const :tag "Pulse" pulse))
  :group 'region-cursors)

(defcustom region-cursors-animation-interval 0.75
  "Animation interval in seconds when `region-cursors-animation' is not `none'."
  :type 'number
  :safe #'numberp
  :group 'region-cursors)

(defcustom region-cursors-animation-delay 0.75
  "Delay in seconds before starting animation after region becomes static.
Set to 0 to start animations immediately (no delay)."
  :type 'number
  :safe #'numberp
  :group 'region-cursors)

(defcustom region-cursors-reset-animation-on-scroll t
  "Whether to reset cursor overlay animations when scrolling occurs."
  :type 'boolean
  :safe #'booleanp
  :group 'region-cursors)

(defcustom region-cursors-scroll-settle-delay 0.25
  "Seconds of scroll inactivity before restarting animation."
  :type 'number
  :safe #'numberp
  :group 'region-cursors)

(defcustom region-cursors-pulse-steps 10
  "Number of steps in pulse animation cycle."
  :type 'integer
  :safe #'integerp
  :group 'region-cursors)

(defcustom region-cursors-disable-hl-line-mode nil
  "Whether to disable `hl-line-mode' while region is active."
  :type 'boolean
  :safe #'booleanp
  :group 'region-cursors)

(defcustom region-cursors-disabled-modes '()
  "Major modes where `region-cursors' should not activate."
  :type '(repeat symbol)
  :group 'region-cursors)

(defface region-cursors-point-face
  '((t :inherit cursor))
  "Face for the point cursor overlay.
Only the `:background' attribute is used.  Inherits the `cursor' face so
that the overlay matches the real cursor and themes may override it.
Set `region-cursors-point-cursor-color' to override it directly."
  :group 'region-cursors)

(defface region-cursors-mark-face
  '((t))
  "Face for the mark cursor overlay.
Only the `:background' attribute is used.  When it specifies no background,
the mark cursor falls back to the point cursor color.
Set `region-cursors-mark-cursor-color' to override it directly."
  :group 'region-cursors)

(defface region-cursors-rectangle-face
  '((t))
  "Face for rectangle corner cursor overlays.
Only the `:background' attribute is used.  When it specifies no background,
the corners fall back to the point cursor color.
Set `region-cursors-rectangle-cursor-color' to override it directly."
  :group 'region-cursors)

(defconst region-cursors--cursor-saved-sentinel :saved)

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

(defvar-local region-cursors--last-region-bounds nil
  "Last known region bounds as (point . mark) to detect changes.")

(defvar-local region-cursors--saved-cursor-type nil
  "Storage for the original cursor type.")

(defvar-local region-cursors--animation-timer nil
  "Timer for cursor overlay animations.")

(defvar-local region-cursors--animation-delay-timer nil
  "Timer that waits before starting animation on static region.")

(defvar-local region-cursors--scroll-timer nil
  "Timer to restart animation after scrolling stops.")

(defvar-local region-cursors--last-window-start nil
  "Last known window start position, used to detect scrolling.")

(defvar-local region-cursors--blink-state t
  "Current visibility state for blinking cursors (t = visible).")

(defvar-local region-cursors--pulse-step 0
  "Current step in pulse animation (0 to pulse-steps).")

(defvar-local region-cursors--pulse-direction 1
  "Direction of pulse animation (1 = brightening, -1 = dimming).")

(defvar-local region-cursors--hl-line-was-active nil
  "Track if `hl-line-mode' was active before region activation.")

(defmacro region-cursors--ensure-overlay! (var pos)
  "Ensure VAR is a live cursor overlay at POS.
If VAR is already a live overlay it is moved to POS; otherwise a new
overlay is created at POS.  VAR is updated in place either way."
  `(setq ,var
         (if (overlayp ,var)
             (progn (region-cursors--move-overlay ,var ,pos) ,var)
           (region-cursors--make-overlay ,pos))))

(defmacro region-cursors--delete-overlay! (var)
  "Delete overlay stored in VAR and set VAR to nil.
VAR must be a symbol naming a variable holding an overlay or nil."
  `(when (overlayp ,var)
     (delete-overlay ,var)
     (setq ,var nil)))

(defun region-cursors--all-overlays ()
  "Return the list of all cursor overlays.
Note that entries may be nil or dead, so callers should guard with `overlayp'."
  (list region-cursors--point-overlay
        region-cursors--mark-overlay
        region-cursors--rect-top-left-overlay
        region-cursors--rect-top-right-overlay
        region-cursors--rect-bottom-left-overlay
        region-cursors--rect-bottom-right-overlay))

(defun region-cursors--resolve-cursor-color ()
  "Return the effective point cursor color."
  (or region-cursors-point-cursor-color
      (face-background 'region-cursors-point-face nil t)
      "#ff6c6b"))

(defun region-cursors--resolve-mark-color ()
  "Return the effective mark cursor color."
  (or region-cursors-mark-cursor-color
      (face-background 'region-cursors-mark-face nil t)
      (region-cursors--resolve-cursor-color)))

(defun region-cursors--resolve-rectangle-color ()
  "Return the effective rectangle cursor color."
  (or region-cursors-rectangle-cursor-color
      (face-background 'region-cursors-rectangle-face nil t)
      (region-cursors--resolve-cursor-color)))

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
     ;; NOTE(abi): tabs get a zero-width overlay (will be rendered with "before-string"),
     ;;            otherwise we cover the character.
     (let* ((start (region-cursors--cursor-anchor pos))
            (end (min (1+ start) (point-max))))
       (cons start end)))
    ('bar
     (cons pos pos))))

(defun region-cursors--hide-cursor ()
  "Hide the real cursor, saving its previous value."
  (unless region-cursors--saved-cursor-type
    (setq region-cursors--saved-cursor-type (or cursor-type region-cursors--cursor-saved-sentinel))
    (setq cursor-type nil)))

(defun region-cursors--restore-cursor ()
  "Restore the real cursor if it was hidden."
  (when region-cursors--saved-cursor-type
    (setq cursor-type
          (if (eq region-cursors--saved-cursor-type region-cursors--cursor-saved-sentinel)
              nil
            region-cursors--saved-cursor-type))
    (setq region-cursors--saved-cursor-type nil)))

(defun region-cursors--cancel-animation-delay-timer ()
  "Cancel the animation delay timer if it exists."
  (when (timerp region-cursors--animation-delay-timer)
    (cancel-timer region-cursors--animation-delay-timer)
    (setq region-cursors--animation-delay-timer nil)))

(defun region-cursors--start-animation-delayed ()
  "Start animation after delay if region remains static."
  (region-cursors--cancel-animation-delay-timer)
  (unless (eq region-cursors-animation 'none)
    (if (<= region-cursors-animation-delay 0)
        (region-cursors--start-animation-timer)
      ;; NOTE(abi): capture buffer so the delay timer callback runs in the correct
      ;;            buffer context, not whatever happens to be current.
      (let ((buf (current-buffer)))
        (setq region-cursors--animation-delay-timer
              (run-with-timer region-cursors-animation-delay nil
                              (lambda ()
                                (when (buffer-live-p buf)
                                  (with-current-buffer buf
                                    (region-cursors--start-animation-timer))))))))))

(defun region-cursors--region-changed-p ()
  "Return non-nil if region bounds have changed since the last check."
  (let ((current-bounds (cons (point) (mark))))
    (if (equal current-bounds region-cursors--last-region-bounds)
        nil
      (progn
        (setq region-cursors--last-region-bounds current-bounds)
        t))))

(defun region-cursors--cancel-animation-timer ()
  "Cancel the animation timer if it exists."
  (when (timerp region-cursors--animation-timer)
    (cancel-timer region-cursors--animation-timer)
    (setq region-cursors--animation-timer nil)))

(defun region-cursors--start-animation-timer ()
  "Start the animation timer if `region-cursors-animation' is not `none'."
  (when (not (eq region-cursors-animation 'none))
    (region-cursors--cancel-animation-timer)
    (setq region-cursors--blink-state t)
    (setq region-cursors--pulse-step region-cursors-pulse-steps)
    (setq region-cursors--pulse-direction -1)
    ;; NOTE(abi): capture buffer so the repeating timer callback runs in
    ;;            the correct buffer context.  Without this, the timer fires
    ;;            in whatever buffer is current and reads the wrong locals.
    (let* ((buf (current-buffer))
           (interval (if (eq region-cursors-animation 'pulse)
                         (/ region-cursors-animation-interval
                            region-cursors-pulse-steps)
                       region-cursors-animation-interval)))
      (setq region-cursors--animation-timer
            (run-with-timer interval interval
                            (lambda ()
                              (if (buffer-live-p buf)
                                  (with-current-buffer buf
                                    (region-cursors--animate-tick))
                                ;; Buffer was killed; cancel ourselves.
                                (region-cursors--cancel-animation-timer))))))))

(defun region-cursors--stop-animation ()
  "Stop animation and reset overlays to full cursor color."
  (region-cursors--cancel-animation-timer)
  (region-cursors--cancel-animation-delay-timer)

  ;; NOTE(abi): restore each overlay's own base color, no blend.
  (region-cursors--apply-color-to-overlays nil t 'restore))

(defun region-cursors--animate-tick ()
  "Update cursor overlays for the current animation frame."
  (condition-case err
      (pcase region-cursors-animation
        ('blink
         (setq region-cursors--blink-state (not region-cursors--blink-state))
         (if region-cursors--blink-state
             (region-cursors--apply-color-to-overlays nil nil t)
           (region-cursors--apply-color-to-overlays
            (region-cursors--get-default-background))))

        ('pulse
         ;; Update pulse step
         (setq region-cursors--pulse-step
               (+ region-cursors--pulse-step region-cursors--pulse-direction))

         ;; Reverse direction at boundaries
         (when (or (>= region-cursors--pulse-step region-cursors-pulse-steps)
                   (<= region-cursors--pulse-step 0))
           (setq region-cursors--pulse-direction (- region-cursors--pulse-direction)))

         ;; Calculate alpha
         ;; NOTE(abi): we hardcode 0.3 to 1.0 for a smooth pulse, but maybe we
         ;;            could expose these to the user.
         (let ((alpha (+ 0.3 (* 0.7 (/ (float region-cursors--pulse-step)
                                       region-cursors-pulse-steps)))))
           (region-cursors--apply-color-to-overlays alpha t))))

    ;; If animation fails, cancel the timer to prevent repeated errors
    (error
     (progn
       (region-cursors--cancel-animation-timer)
       (message "region-cursors animation error: %S" err)))))

(defun region-cursors--on-scroll (window _start-pos)
  "Stop animation when scrolling occurs in WINDOW, restart after it settles."
  (when (and region-cursors-reset-animation-on-scroll
             (eq window (selected-window))
             region-cursors--region-was-active)
    (region-cursors--stop-animation)
    (when (timerp region-cursors--scroll-timer)
      (cancel-timer region-cursors--scroll-timer)
      (setq region-cursors--scroll-timer nil))
    (let ((buf (current-buffer)))
      (setq region-cursors--scroll-timer
            (run-with-timer region-cursors-scroll-settle-delay nil
                            (lambda ()
                              (when (buffer-live-p buf)
                                (with-current-buffer buf
                                  (setq region-cursors--scroll-timer nil)
                                  (region-cursors--start-animation-delayed)))))))))

(defun region-cursors--get-default-background ()
  "Get the default background color for the current frame."
  (or (face-background 'default)
      (frame-parameter nil 'background-color)
      "#000000"))

(defun region-cursors--color-rgb-to-hex (red green blue &optional digits-per-component)
  "Convert RED GREEN BLUE components to hex string.
Each component should be a float between 0.0 and 1.0.
DIGITS-PER-COMPONENT controls precision (defaults to 2, giving #RRGGBB format)."
  (let ((digits (or digits-per-component 2)))
    (format (pcase digits
              (2 "#%02x%02x%02x")
              (4 "#%04x%04x%04x")
              (_ "#%02x%02x%02x"))
            (round (* red (1- (expt 16 digits))))
            (round (* green (1- (expt 16 digits))))
            (round (* blue (1- (expt 16 digits)))))))

(defun region-cursors--blend-colors (color1 color2 alpha)
  "Blend COLOR1 and COLOR2 with ALPHA (0.0 to 1.0).
ALPHA of 0.0 returns COLOR2, 1.0 returns COLOR1."
  (let* ((c1 (color-name-to-rgb color1))
         (c2 (color-name-to-rgb color2))
         (r (+ (* alpha (nth 0 c1)) (* (- 1.0 alpha) (nth 0 c2))))
         (g (+ (* alpha (nth 1 c1)) (* (- 1.0 alpha) (nth 1 c2))))
         (b (+ (* alpha (nth 2 c1)) (* (- 1.0 alpha) (nth 2 c2)))))
    (region-cursors--color-rgb-to-hex r g b 2)))

(defun region-cursors--get-overlay-spacer-color (ov-pos)
  "Get the appropriate spacer color for overlay at OV-POS.
Returns region color if position is at region start, background color if at end."
  (let ((point-pos (point))
        (mark-pos (mark)))
    (if (= ov-pos (min point-pos mark-pos))
        (or (face-background 'region nil t) "#3a3f5a")
      (region-cursors--get-default-background))))

(defun region-cursors--apply-shape (ov &optional color keep-base-color)
  "Apply cursor shape settings to overlay OV with optional COLOR.
If KEEP-BASE-COLOR is non-nil, do not overwrite the stored base color."
  (let* ((pos (overlay-start ov))
         (char-at-pos (char-after pos))
         (is-tab (and char-at-pos (eq char-at-pos ?\t)))
         (cursor-color (or color
                           (overlay-get ov 'region-cursors-color)
                           (region-cursors--resolve-cursor-color))))

    (unless keep-base-color
      (overlay-put ov 'region-cursors-color cursor-color))

    (pcase region-cursors-cursor-shape
      ('box
       (if is-tab
           (let* ((col (save-excursion (goto-char pos) (current-column)))
                  (next-col (save-excursion (goto-char (1+ pos)) (current-column)))
                  (tab-width-chars (- next-col col))
                  (spacer-width (max 0 (1- tab-width-chars)))
                  (spacer-color (region-cursors--get-overlay-spacer-color pos)))
             (overlay-put ov 'display
                          (concat
                           (propertize " "
                                       'face `(:background ,cursor-color))
                           (propertize (make-string spacer-width ?\s)
                                       'face `(:background ,spacer-color))))
             (overlay-put ov 'before-string nil))
         (progn
           (let ((fg (region-cursors--get-default-background)))
             (overlay-put ov 'face `(:background ,cursor-color :foreground ,fg)))
           (overlay-put ov 'display nil)
           (overlay-put ov 'before-string nil))))

      ('bar
       (overlay-put ov 'face nil)
       (overlay-put ov 'display nil)
       (overlay-put ov 'before-string
                    (propertize " "
                                'display `(space :width (,region-cursors-bar-width))
                                'face `(:background ,cursor-color)))))))

(defun region-cursors--apply-color-to-overlays (color-or-alpha &optional is-alpha restore)
  "Apply COLOR-OR-ALPHA to all cursor overlays.
If IS-ALPHA is non-nil, treat as alpha and blend from each overlay's base color.
If RESTORE is non-nil, reset each overlay to its own stored base color."
  (dolist (ov (region-cursors--all-overlays))
    (when (and (overlayp ov) (overlay-buffer ov))
      (let ((color (cond
                    (restore
                     (overlay-get ov 'region-cursors-color))
                    (is-alpha
                     (region-cursors--blend-colors
                      (overlay-get ov 'region-cursors-color)
                      (region-cursors--get-default-background)
                      color-or-alpha))
                    (t color-or-alpha))))
        (region-cursors--apply-shape ov color is-alpha)))))

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
  (region-cursors--delete-overlay! region-cursors--point-overlay)
  (region-cursors--delete-overlay! region-cursors--mark-overlay))

(defun region-cursors--cleanup-rectangle-overlays ()
  "Remove rectangle corner cursor overlays."
  (region-cursors--delete-overlay! region-cursors--rect-top-left-overlay)
  (region-cursors--delete-overlay! region-cursors--rect-top-right-overlay)
  (region-cursors--delete-overlay! region-cursors--rect-bottom-left-overlay)
  (region-cursors--delete-overlay! region-cursors--rect-bottom-right-overlay))

(defun region-cursors--cleanup ()
  "Remove all region cursor overlays."
  (region-cursors--cancel-animation-timer)
  (region-cursors--cancel-animation-delay-timer)
  (when (timerp region-cursors--scroll-timer)
    (cancel-timer region-cursors--scroll-timer)
    (setq region-cursors--scroll-timer nil))
  (region-cursors--cleanup-point-mark-overlays)
  (region-cursors--cleanup-rectangle-overlays))

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

    ;; NOTE(abi): we reapply shape after moving, just in case we land on a tab.
    (unless (timerp region-cursors--animation-timer)
      (region-cursors--apply-shape ov))))

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

(defun region-cursors--deactivate ()
  "Tear down all active region cursor state.
Cleans up overlays, restores cursor and hl-line, and resets tracking variables."
  (region-cursors--cleanup)
  (region-cursors--restore-cursor)
  (region-cursors--restore-hl-line)
  (setq region-cursors--region-was-active nil)
  (setq region-cursors--last-region-bounds nil)
  (setq region-cursors--last-window-start nil))

(defun region-cursors--update-point-mark-overlays ()
  "Create or move point and mark cursor overlays."
  (region-cursors--ensure-overlay! region-cursors--point-overlay (point))
  (region-cursors--ensure-overlay! region-cursors--mark-overlay (mark))
  (overlay-put region-cursors--point-overlay 'region-cursors-color
               (region-cursors--resolve-cursor-color))
  (overlay-put region-cursors--mark-overlay 'region-cursors-color
               (region-cursors--resolve-mark-color))
  (unless (timerp region-cursors--animation-timer)
    (region-cursors--apply-shape region-cursors--point-overlay
                                 (region-cursors--resolve-cursor-color))
    (region-cursors--apply-shape region-cursors--mark-overlay
                                 (region-cursors--resolve-mark-color))))

(defun region-cursors--update-rectangle-overlays ()
  "Create or move rectangle corner overlays."
  (let ((corners (region-cursors--get-rectangle-corners)))
    (if corners
        (progn
          (region-cursors--ensure-overlay! region-cursors--rect-top-left-overlay (nth 0 corners))
          (region-cursors--ensure-overlay! region-cursors--rect-top-right-overlay (nth 1 corners))
          (region-cursors--ensure-overlay! region-cursors--rect-bottom-left-overlay (nth 2 corners))
          (region-cursors--ensure-overlay! region-cursors--rect-bottom-right-overlay (nth 3 corners))
          (region-cursors--cleanup-point-mark-overlays))

      ;; Clean up any stale overlays and fall back to point/mark
      ;; cursors so that we always show something.
      (region-cursors--cleanup-rectangle-overlays)
      (region-cursors--update-point-mark-overlays))))

(defun region-cursors--restrict-overlays-to-window (win)
  "Restrict all live cursor overlays to WIN.
Overlays belong to a buffer, not a window.  This means that they render
in every window showing that buffer.  Setting the `window' overlay property
scopes each cursor to WIN, so that the same buffer displayed in multiple
windows does not show phantom cursors everywhere."
  (dolist (ov (region-cursors--all-overlays))
    (when (and (overlayp ov) (overlay-buffer ov))
      (overlay-put ov 'window win))))

(defun region-cursors--update (&optional window)
  "Update region cursor overlays.
WINDOW is the window being redisplayed (from `pre-redisplay-functions')."
  (cond
   ;; NOTE(abi): clean up unfocused windows, but do skip when the same buffer
   ;;            is shown in the selected window.
   ((and window (not (eq window (selected-window))))
    (when (and region-cursors--region-was-active
               (not (eq (current-buffer) (window-buffer (selected-window)))))
      (region-cursors--deactivate)))

   ((not (apply #'derived-mode-p region-cursors-disabled-modes))
    (let ((region-active (use-region-p))
          (rect-active (region-cursors--rectangle-active-p)))
      (cond
       ;; Activation
       ((and region-active (not region-cursors--region-was-active))
        (setq region-cursors--region-was-active t)
        (region-cursors--hide-cursor)
        (region-cursors--disable-hl-line)
        (setq region-cursors--last-region-bounds (cons (point) (mark)))
        (region-cursors--start-animation-delayed))

       ;; Deactivation
       ((and (not region-active) region-cursors--region-was-active)
        (region-cursors--deactivate)))

      ;; Rendering
      (when region-active
        (region-cursors--hide-cursor)

        ;; IMPORTANT(abi): force cursor to stay hidden, just in case it gets restored.
        (when (and region-cursors--saved-cursor-type cursor-type)
          (setq cursor-type nil))

        ;; NOTE(abi): detect scrolling every frame for immediate animation stop.
        (let ((current-start (window-start)))
          (when (and region-cursors-reset-animation-on-scroll
                     region-cursors--last-window-start
                     (not (= current-start region-cursors--last-window-start))
                     (timerp region-cursors--animation-timer))
            (region-cursors--stop-animation))
          (setq region-cursors--last-window-start current-start))

        (when (region-cursors--region-changed-p)
          (region-cursors--stop-animation)
          (region-cursors--start-animation-delayed))

        (if rect-active
            (region-cursors--update-rectangle-overlays)
          (region-cursors--update-point-mark-overlays)
          (region-cursors--cleanup-rectangle-overlays))

        (region-cursors--restrict-overlays-to-window (or window (selected-window))))))))

(defun region-cursors--reset-and-cleanup (&optional _frame)
  "Perform complete state reset and cleanup of overlays.
Used when switching windows or reverting buffers.
Optional _FRAME argument is ignored (for hook compatibility)."
  (region-cursors--deactivate)
  (deactivate-mark))

;;;###autoload
(define-minor-mode region-cursors-mode
  "Toggle `region-cursors-mode'.

  When enabled, cursor-like overlays replace the real cursor while
  a region is active."
  :global t
  :lighter " rc"
  ;; NOTE(abi): updates happen before redisplay to ensure overlays stay
  ;;            in sync during mouse drag operations and whatnot.
  (if region-cursors-mode
      (progn
        (add-hook 'pre-redisplay-functions #'region-cursors--update)
        (add-hook 'window-selection-change-functions #'region-cursors--reset-and-cleanup)
        (add-hook 'before-revert-hook #'region-cursors--reset-and-cleanup)
        (add-hook 'window-scroll-functions #'region-cursors--on-scroll))
    (remove-hook 'pre-redisplay-functions #'region-cursors--update)
    (remove-hook 'window-selection-change-functions #'region-cursors--reset-and-cleanup)
    (remove-hook 'before-revert-hook #'region-cursors--reset-and-cleanup)
    (remove-hook 'window-scroll-functions #'region-cursors--on-scroll)
    (region-cursors--cleanup)
    (region-cursors--restore-cursor)
    (setq region-cursors--region-was-active nil)))

(provide 'region-cursors)

;;; region-cursors.el ends here
