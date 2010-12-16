(use-modules (srfi srfi-1))
(use-modules (ice-9 pretty-print))

(load "src/misc.ss")
(load "src/reader.ss")
(load "src/expand.ss")
(load "src/transform.ss")

(define build-in-macros (interpret (open-input-file "library/build-in-macros.ss")))
(define build-in-syntax (interpret (open-input-file "library/build-in-syntax.ss")))
(define program (interpret (current-input-port)))

(let ((output (current-output-port)))
  ((if (isatty? output)
     pretty-print
     write)
   (expand (cons 'begin (append build-in-syntax build-in-macros program)))
   output))
(newline)

