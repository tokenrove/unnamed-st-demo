
(defpackage :tilemap-tool
  (:use :cl :anaphora :imago))

(in-package :tilemap-tool)

(defparameter *max-groups* 8)
(defparameter *bitplanes* 3)
(defparameter *block-size* 8)


(defun pcx->palette-scroller-stuff (filename)
  "Outputs data for palette scrolling routine."
  ;; read image
  (let* ((source (read-image filename))
	 (destination (make-instance 'planar-image
				     :width (image-width source)
				     :height (image-height source)
				     :plane-count *bitplanes*))
	 (colors))

    (let ((slack (mod (image-height source) *block-size*)))
      (unless (zerop slack)
	(warn "Ignoring extra ~A lines of image (not aligned on a
boundary of ~A." slack *block-size*)))

    ;; create groupings.  note that we build the colors list in
    ;; reverse (as a stack) and reverse it back to order when
    ;; outputting, below.
    (loop for y from 0 below (image-height source) by *block-size*
	  do (let ((groups (create-groupings source y)))
	       (format t "~&~A: required ~A groups~%" y (length groups))
	       (push (palettize-groupings groups source y) colors)
	       (groups->scanlines destination groups y)))

    ;; make tilemap
    (multiple-value-bind (map tiles width height)
	(make-tilemap destination)
      (format t "~&turned into a ~Ax~A map with ~A tiles."
	      width height (length tiles))

      (with-open-file (stream (replace-extension filename "mapdat")
			      :direction :output
			      :element-type 'unsigned-byte
			      :if-exists :supersede
			      :if-does-not-exist :create)
	;; write header
	(write-big-endian-data stream width 16)
	(write-big-endian-data stream height 16)
	(write-big-endian-data stream (length tiles) 16)
	(format t "~&map: ~X~%" (file-position stream))
	;; write map
	(dotimes (y height)
	  (format t "~&  ")
	  (dotimes (x width)
	    (format t "~2X" (aref map y x))
	    (write-big-endian-data stream (aref map y x) 16)))
	(format t "~&colors: ~X" (file-position stream))
	;; write colors
	(assert (= (length colors) (* height 2)))
	(dolist (color-set (reverse colors))
	  ;; XXX combine flags into color 0.  we don't do this because
	  ;; we don't have actual flag input yet.  they must be set
	  ;; manually by someone with lots of free time, like me.
	  (dotimes (y *block-size*)
	    (dotimes (i *max-groups*)
	      (let ((c (color->atari-pal (aref color-set y i))))
		(write-big-endian-data stream c 16)))))
	(format t "~&tiles: ~X" (file-position stream))
	;; write tiles
	(dotimes (i (length tiles))
	  (let ((tile (aref tiles i)))
	    (format t "~&tile ~A:~%" i)
	    (dotimes (y 16)
	      (format t "~& ~48,B" (aref tile y))
	      (write-big-endian-data stream (aref tile y)
				     (* 16 *bitplanes*)))))))))

;;; XXX eventually I'm going to add stuff so that this adds palette
;;; switching every eight lines. (or every line, hell.)
(defun pcx->chunky-scroller-stuff (filename)
  "Outputs data for chunky scrolling routine."
  ;; read image
  (let* ((source (read-image filename)))
    (let ((slack (mod (image-height source) *block-size*)))
      (unless (zerop slack)
	(warn "Ignoring extra ~A lines of image (not aligned on a
boundary of ~A." slack *block-size*)))

    ;; make tilemap
    (multiple-value-bind (map tiles width height) (make-tilemap source)
      (format t "~&turned into a ~Ax~A map with ~A tiles."
	      width height (length tiles))

      (with-open-file (stream (replace-extension filename "mapdat")
			      :direction :output
			      :element-type 'unsigned-byte
			      :if-exists :supersede
			      :if-does-not-exist :create)
	;; write header
	(write-big-endian-data stream width 16)
	(write-big-endian-data stream height 16)
	(write-big-endian-data stream (length tiles) 16)
	(format t "~&map: ~X~%" (file-position stream))
	;; write map
	(dotimes (y height)
	  (dotimes (x width)
	    (write-big-endian-data stream (aref map y x) 16)))
	(format t "~&colors: ~X" (file-position stream))
	;; write colors
	(dotimes (i 16)
	  (let ((c (color->atari-pal (aref (image-colormap source) i))))
	    (write-big-endian-data stream c 16)))
	(format t "~&tiles: ~X" (file-position stream))
	;; write tiles
	(dotimes (i (length tiles))
	  (let ((tile (aref tiles i)))
	    (dotimes (y 16)
	      (write-big-endian-data stream (aref tile y)
				     (* 16 *bitplanes*)))))))))

(defun color->atari-pal (color)
  (flet ((just-3-bits (c) (ldb (byte 3 5) c)))
    (logior (ash (just-3-bits (color-red color)) 8)
	    (ash (just-3-bits (color-green color)) 4)
	    (just-3-bits (color-blue color)))))


(defun palettize-groupings (groups image base-y)
  (let ((colors (make-array `(,*block-size* ,*max-groups*)
			    :initial-element 0)))
    (dotimes (y *block-size*)
      (loop for g from 0 below (length groups)
	    for group in groups
	    do (setf (aref colors y g) (aref (image-colormap image)
					     (image-pixel image
							  (first group)
							  (+ base-y y))))))
    colors))

;; Returns tilemap, tile palette, width and height in tiles.
(defun make-tilemap (image)
  (let ((map (make-array `(,(floor (image-height image) 16)
			   ,(floor (image-width image) 16))))
	(tiles (make-hash-table :test #'equalp)))
    (flet ((serialize-tile (x y)
	     (let ((result (make-array '(16)
				       :element-type `(unsigned-byte ,(* 16 *bitplanes*)))))
	       (do-region-pixels (image pel x y x y 16 16)
		 ;; Fucking interleaved planes.
		 (dotimes (p *bitplanes*)
		   (setf (ldb (byte 1 (+ (- 15 (mod x 16))
					 (* (- (1- *bitplanes*) p) 16)))
			      (aref result (mod y 16)))
			 (ldb (byte 1 p) pel))))
	       result)))
      ;; for each 16x16 block of the image, read the chunk, see if it's
      ;; already in the hash table or not.  write an index to map (x,y).
      (dotimes (y (array-dimension map 0))
	(dotimes (x (array-dimension map 1))
	  (let ((tile (serialize-tile (* x 16) (* y 16))))
	    (setf (aref map y x)
		  (sif (gethash tile tiles)
		       it
		       (setf it (hash-table-count tiles))))))))
    (let ((serial-tiles (make-array `(,(hash-table-count tiles)))))
      (maphash (lambda (k v) (setf (aref serial-tiles v) k)) tiles)
      (values map serial-tiles
	      (array-dimension map 1) (array-dimension map 0)))))

(defun create-groupings (image base-y)
  (let ((groups nil)
	(ungrouped (loop for i below (image-width image)
			 collecting i)))
    (labels ((all-pixels-of-color (target line)
	       (let ((result nil))
		 (do-region-pixels (image color x y 0 line
					  (image-width image) 1)
		   (when (eql color target) (push x result)))
		 (intersection ungrouped result)))
	     (new-group ()
	       (when ungrouped
		 (all-pixels-of-color (image-pixel image (first ungrouped)
						   base-y)
				      base-y)))
	     (colors-mismatch (xs y)
	       (mismatch (mapcar (lambda (x) (image-pixel image x y)) xs)
			 (mapcar (constantly (image-pixel image (first xs) y))
				 xs))))
      (do ((group (new-group) (new-group)))
	  ((or (null ungrouped) (= (length groups) *max-groups*)))
	(do* ((y (1+ base-y) (1+ y))
	      (mismatch (colors-mismatch group y)
			(colors-mismatch group y)))
	     ((= y (+ (1- *block-size*) base-y)))
	  (when mismatch
	    (setf group (delete (elt group mismatch) group :count 1))
	    (setf y (1+ base-y))))
	(push group groups)
	(setf ungrouped (set-difference ungrouped group))))

    (unless (null ungrouped)
      (error "Image can't be converted -- too much going on around scanline ~A"
	     base-y))

    ;; Hopefully this puts the biggest chunk on color 0, which is
    ;; almost always what we want.
    (setf groups (sort groups #'>= :key #'length))
    groups))

(defun groups->scanlines (image groups base-y)
  (loop for y from base-y below (+ base-y *block-size*)
	do (loop for g from 0 below (length groups)
		 for group in groups
		 do (dolist (x group)
		      (setf (image-pixel image x y) g)))))


