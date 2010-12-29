(use-modules (r5rs))
(use-modules (srfi srfi-1))
(use-modules (yume misc))
(use-modules (yume reader))
(use-modules (yume syntax))
(use-modules (yume expand))
;(use-modules (yume transform))

(define build-in-macros (interpret (open-input-file "library/build-in-macros.ss")))
(define build-in-syntax (interpret (open-input-file "library/build-in-syntax.ss")))
(define program (interpret (current-input-port)))

(write
  (call-with-values
    (lambda () (macros program))
    (lambda (inline-macros statements)
      (expand
	(append
	  build-in-syntax
	  build-in-macros
	  inline-macros)
	(cons 'begin statements)))))
