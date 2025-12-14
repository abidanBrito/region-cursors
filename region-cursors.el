;;; region-cursors.el --- Display point and active mark using cursors -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Abidán Brito Clavijo

;; Author: Abidán Brito Clavijo <abidan.brito@gmail.com>
;; Version: 1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: convenience, faces
;; URL: https://github.com/abidanBrito/region-cursors
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This package provides a minor mode that enhances cursor display when
;; a region is active.  When mark is set and region is active, both the
;; point and mark positions are highlighted with distinct colors.  When
;; no region is active, the standard cursor is used.

;;; Code:

(defgroup region-cursors nil
  "Character-based cursor display for active regions."
  :group 'editing
  :group 'convenience)

(defcustom region-cursors-point-color (face-background 'cursor)
  "Color for the point cursor when region is active."
  :type 'color
  :group 'region-cursors)

(defcustom region-cursors-mark-color (face-background 'cursor)
  "Color for the mark cursor when region is active."
  :type 'color
  :group 'region-cursors)

(defcustom region-cursors-use-cursor-without-region nil
  "If non-nil, use cursors when no region is active."
  :type 'boolean
  :group 'region-cursors)

(defvar region-cursors--point-overlay nil
  "Overlay for the point position.")

(defvar region-cursors--mark-overlay nil
  "Overlay for the mark position.")

(defvar region-cursors--original-cursor-type nil
  "Storage for original cursor type.")

(defvar region-cursors--region-was-active nil
  "Track if region was active in the previous update.")

;;;###autoload
(define-minor-mode region-cursors-mode
  "Toggle cursor display for active regions.
When enabled and a region is active, both the point and mark positions
are highlighted with distinct colors using overlays.  When no region is
active, the standard cursor is displayed instead."
  :global t
  :group 'region-cursors
  :lighter " ●○"
  (if region-cursors-mode
      (region-cursors--enable)
    (region-cursors--disable)))

(defun region-cursors--region-active-p ()
  "Check if region is truly active and valid."
  (and (region-active-p)
       (mark)
       (/= (point) (mark))))

(defun region-cursors--create-overlay (pos color)
  "Create an overlay at POS with COLOR.
Returns the created overlay."
  (let* ((end-pos (min (1+ pos) (point-max)))
         (ov (make-overlay pos end-pos))
         (bg-color (face-background 'default)))

    ;; Only use after-string at end of buffer when necessary
    (if (and (= pos (point-max)) (= pos end-pos))
        (progn
          (overlay-put ov 'after-string
                       (propertize " " 'face
                                   `(:background ,color :foreground ,bg-color)))
          ;; Don't apply face to the overlay itself at EOB
          (overlay-put ov 'face nil))
      ;; Normal case: apply face to the character
      (overlay-put ov 'face
                   `(:background ,color :foreground ,bg-color)))

    (overlay-put ov 'priority 1001)
    (overlay-put ov 'window (selected-window))
    ov))

(defun region-cursors--cleanup-overlays ()
  "Remove all cursor overlays."
  (when (overlayp region-cursors--point-overlay)
    (delete-overlay region-cursors--point-overlay))
  (when (overlayp region-cursors--mark-overlay)
    (delete-overlay region-cursors--mark-overlay))
  (setq region-cursors--point-overlay nil)
  (setq region-cursors--mark-overlay nil))

(defun region-cursors--update-overlays (&rest _)
  "Update cursor overlays based on current state."
  (when region-cursors-mode
    (let ((region-active (region-cursors--region-active-p)))
      (cond
       ;; Region is active: show both overlays, hide normal cursor
       (region-active
        (region-cursors--cleanup-overlays)
        (setq region-cursors--point-overlay
	      (region-cursors--create-overlay (point) region-cursors-point-color))
        (setq region-cursors--mark-overlay
	      (region-cursors--create-overlay (mark) region-cursors-mark-color))
        (when (not region-cursors--region-was-active)
          (setq cursor-type nil)
          (setq region-cursors--region-was-active t)))
       
       ;; No region but user wants cursor anyway
       ((and region-cursors-use-cursor-without-region
             (not region-active))
        (region-cursors--cleanup-overlays)
        (setq region-cursors--point-overlay
	      (region-cursors--create-overlay (point) region-cursors-point-color))
        (when region-cursors--region-was-active
          (setq cursor-type nil)
          (setq region-cursors--region-was-active nil)))
       
       ;; No region and normal cursor desired: clean up and restore
       (t
        (region-cursors--cleanup-overlays)
        (when (or region-cursors--region-was-active
                  (null cursor-type))
          (setq cursor-type (or region-cursors--original-cursor-type t))
          (setq region-cursors--region-was-active nil)))))))

(defun region-cursors--window-change-function (frame)
  "Update cursor overlays upon any window selection change in FRAME."
  (when region-cursors-mode
    (with-selected-frame frame
      (region-cursors--update-overlays))))

(defun region-cursors--enable ()
  "Enable region cursors mode."
  ;; Store original cursor type only if not already stored
  (unless region-cursors--original-cursor-type
    (setq region-cursors--original-cursor-type cursor-type))
  
  ;; Initial update
  (region-cursors--update-overlays)
  
  ;; Setup hooks
  (add-hook 'post-command-hook #'region-cursors--update-overlays)
  (add-hook 'activate-mark-hook #'region-cursors--update-overlays)
  (add-hook 'deactivate-mark-hook #'region-cursors--update-overlays)
  (add-hook 'window-selection-change-functions #'region-cursors--window-change-function))

(defun region-cursors--disable ()
  "Disable region cursors mode."
  ;; Restore original cursor
  (setq cursor-type (or region-cursors--original-cursor-type t))
  
  ;; Clean up overlays
  (region-cursors--cleanup-overlays)
  
  ;; Remove hooks
  (remove-hook 'post-command-hook #'region-cursors--update-overlays)
  (remove-hook 'activate-mark-hook #'region-cursors--update-overlays)
  (remove-hook 'deactivate-mark-hook #'region-cursors--update-overlays)
  (remove-hook 'window-selection-change-functions #'region-cursors--window-change-function)
  
  ;; Reset state
  (setq region-cursors--region-was-active nil))

;;;###autoload
(defun region-cursors-toggle ()
  "Toggle region cursors mode."
  (interactive)
  (region-cursors-mode (if region-cursors-mode -1 1)))

(provide 'region-cursors)

;;; region-cursors.el ends here
