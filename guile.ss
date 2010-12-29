(use-modules (srfi srfi-1))
(use-syntax (ice-9 syncase))
(use-modules (ice-9 pretty-print))

(load "library/match.scm")
(load "src/misc.ss")
(load "src/reader.ss")
(load "src/syntax.ss")
(load "src/expand.ss")
(load "src/transform.ss")

(define build-in-macros (interpret (open-input-file "library/build-in-macros.ss")))
(define build-in-syntax (interpret (open-input-file "library/build-in-syntax.ss")))
(define program (interpret (current-input-port)))

(let ((output (current-output-port)))
  ((if (isatty? output) pretty-print write)
   (call-with-values
     (lambda () (macros program))
     (lambda (inline-macros statements)
       ;(cons inline-macros statements)
       (expand (append build-in-syntax build-in-macros inline-macros) (cons 'begin statements))
       ))
   output)
  (newline output))

