
(defun do-build (out files)
  (flet ((.s->.o (x) (cl-ppcre:regex-replace "\\.s" x ".o")))
    (dolist (x files)
      (m68k-asm:assemble x :object-name (.s->.o x)))
    (st-linker:link (mapcar #'.s->.o files) :out-name out)))

;; I usually symlink the files I'm using from other projects (ymamoto,
;; ssprites stuff, et cetera) into the current directory to make
;; things a little cleaner.
(do-build "../../atari_st/st-demo.prg"
  (list "main.s" "ymamoto.s" "chunky-scroll.s"
	"palette-scroll.s" "t16-3blt.s" "random.s"
	"fade.s" "initutil.s" "font.s" "bomb.s"
	"slut.s"))

(defun print-reciprocal-table (stream)
  (loop for i from 0 below 128
	summing
	(progn
	     (format t "~&  DC.B ")
	     (loop for j from 1 upto i
		   summing 1
		   do (format stream "~D, " (floor i j))))))

