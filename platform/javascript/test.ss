(use-modules (r5rs))
(use-modules (srfi srfi-1))
(use-modules (yume reader))
(use-modules (yume expand))
(use-modules (yume expand))
;(use-modules (yume transform))

(define build-in-macros (interpret (open-input-file "library/build-in-macros.ss")))
(define build-in-syntax (interpret (open-input-file "library/build-in-syntax.ss")))
(define program (interpret (current-input-port)))

(write (expand (cons 'begin (append build-in-syntax build-in-macros program))))

