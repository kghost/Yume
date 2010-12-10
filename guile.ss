(use-modules (srfi srfi-1))
(use-modules (ice-9 pretty-print))

(load "interpreter/reader.ss")
(load "expander/expand.ss")
(load "transformer/transform.ss")

(define build-in-macros (interpret (open-input-file "library/build-in-macros.ss")))
(define program (interpret (current-input-port)))

(let ((output (current-output-port)))
  ((if (isatty? output)
     pretty-print
     write)
   (transform (expand (cons 'begin (append build-in-macros program))))
   output))
(newline)

