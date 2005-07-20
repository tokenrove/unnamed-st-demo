
(defun do-build (out files)
  (flet ((.s->.o (x) (cl-ppcre:regex-replace "\\.s" x ".o")))
    (dolist (x files)
      (m68k-asm:assemble x :object-name (.s->.o x)))
    (st-linker:link (mapcar #'.s->.o files) :out-name out)))

;; I usually symlink the files I'm using from other projects (ymamoto,
;; ssprites stuff, et cetera) into the current directory to make
;; things a little cleaner.
(do-build "../../atari_st/st-demo.prg"
  (list "main.s" "ymamoto.s" "chunky-scroll.s"))
