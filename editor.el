;

(setq tiles nil) ; contains slices for the image
(setq dave-im nil) ; contains the image

(defun create-tiles ()
  (setq tiles nil)
  (cl-loop for i from 0 to 7 do
           (push `(,(* i 16) 0 16 16) tiles))
  (setq tiles (reverse tiles)))

(defun draw-level()
  (interactive)
  (when (eq dave-im nil)
      (setq dave-im (create-image "/home/dave/Dev/panic/RES/TILE.PNG"))
      (message "Image loaded: %s" (image-size dave-im t)))
  (when (eq tiles nil)
    (create-tiles))
  (cl-loop for i from 0 to 7 do
           (insert-image dave-im nil nil (nth i tiles))))

