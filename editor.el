;; Mountain panic level editor

(defvar panic-screens nil "List of screens")
(defvar panic-screens-info nil "List of information for each screen, currently just tileset")
(defvar panic-current-screen 0 "Current screen we are editing")
(defvar panic-tiles1 nil "A list of tile image slices")
(defvar panic-tiles2 nil "A list of tile image slices")
(defvar panic-tile-image nil "Image containing the graphics")
(defvar panic-tile-image-flipped nil "Image containing the graphics (flipped)")
(defvar panic-blank-tile nil "A blank tile")
(defconst panic-scale 1 "Scale of the tiles")
(defconst panic-exported-cell-regexp "\\([0-9][0-9]\\)...." "Regexp to match the exported cell data from `parse-panic-data'")
(defconst panic-row-count 12 "Count of rows in a screen")
(defconst panic-col-count 8 "Count of columns in a screen")

(if (or (string-equal system-type "ms-dos") (string-equal system-type "windows-nt"))
    (defconst panic-parse-tool "parse-panic-data.exe")
  (defconst panic-parse-tool "./parse-panic-data"))

(defvar panic-editor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "j") 'panic-jump-to-screen)
    (define-key map (kbd "n") 'panic-next-screen)
    (define-key map (kbd "p") 'panic-prev-screen)
    (define-key map (kbd "SPC") 'panic-toggle-cell-flag)
    (define-key map (kbd "RET") 'panic-edit-cell)
    (define-key map (kbd "<up>") 'panic-up)
    (define-key map (kbd "<down>") 'panic-down)
    (define-key map (kbd "<left>") 'panic-left)
    (define-key map (kbd "<right>") 'panic-right)
    (define-key map (kbd "q") #'(lambda() (interactive) (kill-buffer "*Panic Editor*")))
    map))

(define-derived-mode panic-editor-mode fundamental-mode "Panic Edit")

(cl-defun panic-tile-message()
  (when (> (line-number-at-pos) panic-row-count)
    (cl-return-from panic-tile-message nil))
  (when (> (current-column) (1- panic-col-count))
    (cl-return-from panic-tile-message nil))
  (let ((row (1- (line-number-at-pos)))
        (col (current-column)))
    (message (nth 1 (nth col (nth row (nth panic-current-screen panic-screens)))))))

(defun panic-up()
  (interactive)
  (forward-char (- -1 panic-col-count))
  (panic-tile-message))

(defun panic-down()
  (interactive)
  (forward-char (1+ panic-col-count))
  (panic-tile-message))

(defun panic-left()
  (interactive)
  (left-char)
  (panic-tile-message))

(defun panic-right()
  (interactive)
  (right-char)
  (panic-tile-message))

(defun panic-write-screen-header(scr)
  "Write a screen's header data into the export buffer"
  (insert "    EQUB &00\n")
  (insert "    EQUB &00\n")
  (insert "    EQUB &00\n")
  (insert "    EQUB &00,&000,&000,&00\n")
  (insert "    EQUB &00\n\n"))

(defun panic-write-screen(scr)
  "Write a screen's bytes into the export buffer")

(defun panic-write-map-data()
  "Write map data into the export buffer"
  (interactive)
  (when (= (panic-total-screens) 0)
    (error "No screens"))
  (with-current-buffer (get-buffer-create "*panic export*")
    (erase-buffer)
    (insert "NUM_SCREENS = " (number-to-string (panic-total-screens)) "\n\n.mapData:\n")
    (insert "    EQUB &00               ; Top 3 bits are tile set number, remaining 5 are index into string table\n")
    (insert "    EQUB &00               ; Index into screen table\n")
    (insert "    EQUB &00               ; FX bits (5) | Item present bit - cleared when this screen's item is collected  (1 bit) | Number aliens present (2 bits)\n")
    (insert "    EQUB &00,&000,&000,&00 ; Screen exits NSEW\n")
    (insert "    EQUB &00               ; 5 bits spare(?) | Screen has played sanity effect (1 bit) | Screen should do sanity loss effect (1 bit) | Screen has item (1 bit)\n\n")
    (cl-loop for i from 1 below (panic-total-screens) do
             (panic-write-screen-header i))))

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

(defun panic-create-blank-tile()
  (setq panic-blank-tile `(,(- 128 16) ,(- 208 16) 16 16)))

(defun panic-draw-screen(arg)
  (when (eq panic-tile-image nil)
    (setq panic-tile-image (create-image (expand-file-name "../panic/RES/TILE.PNG") 'png nil :scale panic-scale)))
  (when (eq panic-tile-image-flipped nil)
    (setq panic-tile-image-flipped (create-image (expand-file-name "../panic/RES/TILE.PNG") 'png nil :scale panic-scale :flip t)))
  (when (eq panic-tiles1 nil)
    (panic-create-tiles1))
  (when (eq panic-tiles2 nil)
    (panic-create-tiles2))
  (when (eq panic-blank-tile nil)
    (panic-create-blank-tile))
  (erase-buffer)
  (let ((scr (nth arg panic-screens))
        (tls (nth arg panic-screens-info)))
    (cl-loop for i from 0 below (length scr) do
             (cl-loop for j in (nth i scr) do
                      (let ((idx (car j))
                            (flg (cadr j)))
                        (if (= idx -1)
                            (insert-image panic-tile-image nil nil panic-blank-tile)
                          (if (= tls 0)
                              (if (string-match-p "X" flg) ; Check flipped flag
                                  (insert-image panic-tile-image nil nil (nth idx panic-tiles1))
                                (insert-image panic-tile-image nil nil (nth idx panic-tiles1)))
                            (insert-image panic-tile-image nil nil (nth idx panic-tiles2))))))
             (newline))))

(defun panic-edit-cell()
  "Edit a cell's index"
  (interactive)
  (read-number "Index (0-31): "))

(defun panic-toggle-cell-flag()
  "Edit a cell's flags"
  (interactive)
  (completing-read "Toggle flag: " '("Hookable" "Ladder" "Collidable") nil t "^" ))

(defun panic-edit-string-table()
  "Edit the string table")

(defun panic-next-screen()
  "Move to the next screen"
  (interactive)
  (when (> (panic-total-screens) 0)
    (when (< panic-current-screen (panic-total-screens))
      (setq panic-current-screen (1+ panic-current-screen))
      (panic-draw-screen panic-current-screen))))

(defun panic-prev-screen()
  "Move to the previous screen"
  (interactive)
  (when (> (panic-total-screens) 0)
    (when (> panic-current-screen 0)
      (setq panic-current-screen (1- panic-current-screen))
      (panic-draw-screen panic-current-screen))))

(defun panic-jump-to-screen(arg)
  "Jump to a screen"
  (interactive "nJump to screen: ")
  (when (and (>= arg 0) (< arg (panic-total-screens)))
    (setq panic-current-screen arg)
    (panic-draw-screen panic-current-screen)))

(defun panic-reset()
  (setq panic-tiles1 nil)
  (setq panic-tiles2 nil)
  (setq panic-blank-tile nil)
  (setq panic-tile-image nil)
  (setq panic-tile-image-flipped nil)
  (setq panic-screens nil)
  (setq panic-screens-info nil))

(defun panic-editor()
  "Start editing data for Mountain Panic 2"
  (interactive)
  (panic-reset)
  (if (= 0 (panic-load-data))
      (progn
        (message "Data parse error")
        nil)
    (progn
      (switch-to-buffer (get-buffer-create "*Panic Editor*"))
      (setq panic-current-screen 0)
      (panic-draw-screen panic-current-screen) ; TODO
      (panic-editor-mode)
      (message "Move around with cursor keys; RET to edit cell; SPC edit flags; s to save; j jump to screen; q to quit")
      t)))

(defun panic-load-screen(arg)
  "Load a screen from the original assembler data."
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
                                  (push (list -1 "....") new-row)
                                (if (string-match panic-exported-cell-regexp i)
                                    (push (list (string-to-number (substring i (match-beginning 1) (match-end 1))) (substring i (match-end 1) (length i))) new-row)
                                  (push (list -1 "....") new-row))))
                     (setq new-row (reverse new-row))
                     (push new-row new-screen))
                   (forward-line))
          (reverse new-screen))
      nil)))

(defun panic-total-screens()
  (if (= 0 (length panic-screens))
      0
    (1- (length panic-screens))))

(cl-defun panic-load-data()
  "Loads level data by creating new output.json and parsing it. Returns number of screens parsed."
  (setq panic-screens nil)
  (setq panic-screens-info nil)
  (when (/= 0 (shell-command panic-parse-tool))
    (cl-return-from panic-load-data 0))
  (let ((pr (make-progress-reporter "Working..."))
        (l (funcall (lambda()
                      (with-current-buffer
                          (if (< emacs-major-version 27)
                              "*Shell Command Output*"
                            shell-command-buffer-name)
                        (goto-char (point-min))
                        (if (search-forward-regexp "Written \\([0-9]+\\) screens" nil t)
                            (string-to-number (buffer-substring (match-beginning 1) (match-end 1)))
                          0))))))
    (cl-loop for i below l do
             (let ((s (panic-load-screen i)))
               (when s
                 (progress-reporter-update pr)
                 (push (funcall (lambda()
                                  (with-current-buffer "*panic parse output*"
                                    (goto-char (point-min))
                                    (string-to-number (buffer-substring (search-forward-regexp "Tileset *: *" nil t 1) (line-end-position))))))
                       panic-screens-info)
                 (push s panic-screens))))
    (setq panic-screens (reverse panic-screens))
    (setq panic-screens-info (reverse panic-screens-info))
    (progress-reporter-done pr))
  (length panic-screens))

(defun panic-save-data()
  "Saves level data")

