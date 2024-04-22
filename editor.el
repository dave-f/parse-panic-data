;; Mountain panic level editor

(defvar panic-screens nil "List of screens")
(defvar panic-current-screen 0 "Current screen we are editing")
(defvar panic-tiles1 nil "A list of tile image slices")
(defvar panic-tiles2 nil "A list of tile image slices")
(defvar panic-tile-image nil "Image containing the graphics")
(defconst panic-scale 1 "Scale of the tiles")
(defconst panic-exported-cell-regexp "\\([0-9][0-9]\\)...." "Regexp to match the exported cell data from `parse-panic-data'")
(defconst panic-row-count 12 "Count of rows in a screen")
(defconst panic-col-count 8 "Count of columns in a screen")

(if (or (string-equal system-type "ms-dos") (string-equal system-type "windows-nt"))
    (defconst panic-parse-tool "parse-panic-data.exe")
  (defconst panic-parse-tool "./parse-panic-data"))

(defvar panic-editor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'panic-next-screen)
    (define-key map (kbd "p") 'panic-prev-screen)
    (define-key map (kbd "q") #'(lambda() (interactive) (kill-buffer "*Panic Editor*")))
    map))

(define-derived-mode panic-editor-mode fundamental-mode "Panic Edit")

;; We no longer have tilesets so just convert the tile index
(defun tileset-index-to-tile-offset(index)
  (if (< index 8)
      0
    -1))

(defun panic-create-tiles1()
  (setq panic-tiles1 nil)
  (cl-loop for i from 0 below 8 do
           (push `(,(* i (* 16 panic-scale)) 0 ,(* 16 panic-scale) ,(* 16 panic-scale)) panic-tiles1))
  (cl-loop for i from 0 below 4 do
           (push `(,(* i (* 16 panic-scale)) 32 ,(* 16 panic-scale) ,(* 16 panic-scale)) panic-tiles1))
  (setq panic-tiles1 (reverse panic-tiles1)))

(defun panic-create-tiles2()
  (setq panic-tiles2 nil)
  (cl-loop for i from 0 below 8 do
           (push `(,(* i (* 16 panic-scale)) 16 ,(* 16 panic-scale) ,(* 16 panic-scale)) panic-tiles2))
  (cl-loop for i from 0 below 4 do
           (push `(,(* i (* 16 panic-scale)) 32 ,(* 16 panic-scale) ,(* 16 panic-scale)) panic-tiles2))
  (setq panic-tiles2 (reverse panic-tiles2)))

(defun panic-draw-screen(arg)
  (when (eq panic-tile-image nil)
    (setq panic-tile-image (create-image (expand-file-name "../panic/RES/TILE.PNG") 'png nil :scale panic-scale))
    (message "Image loaded: %s" (image-size panic-tile-image t)))
  (when (eq panic-tiles1 nil)
    (panic-create-tiles1))
  (when (eq panic-tiles2 nil)
    (panic-create-tiles2))
  (erase-buffer)
  (let ((scr (panic-load-screen arg))
        (flg (panic-create-flags))
        (tls (panic-get-tileset)))
    (cl-loop for i from 0 below (length scr) do
             ;(insert (format "%02d " i))
             (cl-loop for j in (nth i scr) do
                      (if (= tls 0)
                          (insert-image panic-tile-image nil nil (nth j panic-tiles1))
                        (insert-image panic-tile-image nil nil (nth j panic-tiles2))))
             (newline))))

(defun panic-create-screen()
  (make-list panic-row-count '(0 0 0 0 0 0 0 0)))

(defun panic-create-flags()
  (make-list panic-row-count '(0 0 0 0 0 0 0 0)))

(defun panic-get-tileset()
  (with-current-buffer "*panic parse output*"
    (goto-char (point-min))
    (string-to-number (buffer-substring (search-forward "Tileset: " nil t 1) (line-end-position)))))

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

(defun panic-next-screen()
  "Move to the next screen"
  (interactive)
  (when (< panic-current-screen 46)
    (setq panic-current-screen (1+ panic-current-screen))
    (panic-draw-screen panic-current-screen)))

(defun panic-prev-screen()
  "Move to the previous screen"
  (interactive)
  (when (> panic-current-screen 0)
    (setq panic-current-screen (1- panic-current-screen))
    (panic-draw-screen panic-current-screen)))

;; Reset the variables for debugging
(defun panic-reset()
  (setq panic-tiles1 nil)
  (setq panic-tiles2 nil)
  (setq panic-tile-image nil))

;; Main entry
(defun panic-editor()
  "Start editing data for Mountain Panic 2"
  (interactive)
  (panic-reset)
  (panic-load-data)
  (switch-to-buffer (get-buffer-create "*Panic Editor*"))
  (setq panic-current-screen 0)
  (panic-draw-screen panic-current-screen)
  (panic-editor-mode)
  (message "Move around with cursor keys, RET to edit cell, s to save, q to quit, j jump to screen"))

;; Load a screen from the original assembly data
(defun panic-load-screen(arg)
  "Load a screen from the original assembler data"
  (interactive "nLoad screen: ")
  (shell-command (concat panic-parse-tool " -n=" (number-to-string arg)) "*panic parse output*")
  (with-current-buffer "*panic parse output*"
    (goto-char (point-min))
    (if (> (count-lines (point-min) (point-max)) panic-row-count)
        (let (new-screen)
          (cl-loop for i from 0 below panic-row-count do
                   (let ((cells (string-split (buffer-substring (line-beginning-position) (line-end-position))))
                         (new-row nil))
                     (cl-loop for i in cells do
                              (if (string= i "......")
                                  (push 7 new-row)
                                (progn
                                  (if (string-match panic-exported-cell-regexp i)
                                      (push (string-to-number (substring i (match-beginning 1) (match-end 1))) new-row) 
                                    (push 7 new-row)))))
                     (setq new-row (reverse new-row))
                     (push new-row new-screen)
                     (forward-line)))
          (reverse new-screen))
      (message "Not enough lines"))))

;; Load data
(defun panic-load-data()
  "Loads level data")

;; Save data
(defun panic-save-data()
  "Saves level data")


;; TODO
;; (setq level-data (json-read-file (expand-file-name "./output.json")))
;; (length (cdr (nth 1 level-data))) ; 50 screens

