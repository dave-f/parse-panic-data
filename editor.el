;; Mountain panic level editor

(defvar panic-screens nil "List of screens")
(defvar panic-current-screen 1 "Current screen we are editing")
(defvar panic-tiles nil "A list of tile image slices")
(defvar panic-tile-image nil "Image containing the graphics")
(defconst panic-scale 2 "Scale of the tiles")

;; We no longer have tilesets so just convert the tile index
(defun tileset-index-to-tile-offset(index)
  (if (< index 8)
      0
    -1))

(defun panic-create-tiles()
  (setq panic-tiles nil)
  (cl-loop for i from 0 to 7 do
           (push `(,(* i (* 16 panic-scale)) 0 ,(* 16 panic-scale) ,(* 16 panic-scale)) panic-tiles))
  (setq panic-tiles (reverse panic-tiles)))

(defun panic-draw-screen()
  (when (eq panic-tile-image nil)
    (setq panic-tile-image (create-image (expand-file-name "../panic/RES/TILE.PNG") 'png nil :scale panic-scale))
    (message "Image loaded: %s" (image-size panic-tile-image t)))
  (when (eq panic-tiles nil)
    (panic-create-tiles))
  (let ((scr (panic-create-screen))
        (flg (panic-create-flags)))
    (cl-loop for i from 0 below (length scr) do
             (insert (format "%02d " i))
             (cl-loop for j in (nth i scr) do
                      (insert-image panic-tile-image nil nil (nth j panic-tiles)))
             (newline))))

(defun panic-create-screen()
  (make-list 12 '(0 0 0 0 0 0 0 0)))

(defun panic-create-flags()
  (make-list 12 '(0 0 0 0 0 0 0 0)))

;; Edit a cell's index, will be bound to RET
(defun panic-edit-cell()
  "Edit an index"
  ;(interactive)
  )

;; Edit a cell's flags
(defun panic-edit-cell-flags()
  "Edit a cell flags"
  ;(interactive)
  (completing-read "Flags:" '("Hookable" "Climbable" "Collidable" "Flipped")))

;; Edit the string table
(defun panic-edit-string-table()
  "Edit the string table")

;; Reset the variables for debugging
(defun panic-reset()
  (setq panic-tiles nil)
  (setq panic-tile-image nil))

;; Main entry
(defun panic-editor()
  "Start editing data for Mountain Panic 2"
  (interactive)
  (panic-reset)
  (switch-to-buffer (get-buffer-create "*Panic Editor*"))
  (erase-buffer)
  (panic-draw-screen)
  (message "Move around with cursor keys, RET to edit cell, s to save, q to quit, j jump to screen"))

;; Load data
(defun panic-load-data()
  "Loads level data")

;; TODO
;; (setq level-data (json-read-file (expand-file-name "./output.json")))
;; (length (cdr (nth 1 level-data))) ; 50 screens

