(use-modules (r5rs))
(use-modules (srfi srfi-1))
(use-modules (yume misc))
(use-modules (yume reader))
(use-modules (yume syntax))
(use-modules (yume expand))
(use-modules (yume transform))
(use-modules (yume javascript compile))

(define build-in-macros (interpret (open-input-file "library/build-in-macros.ss")))
(define build-in-syntax (interpret (open-input-file "library/build-in-syntax.ss")))
(define program (interpret (current-input-port)))

(let ((output (current-output-port)))
  (compile
    output
    (transform
      (call-with-values
	(lambda () (macros program))
	(lambda (inline-macros statements)
	  (let ((expanded (expand
			    (append
			      build-in-syntax
			      build-in-macros
			      inline-macros)
			    (cons 'begin statements)))
		(expanded-port (expanded-port)))
	    (write expanded expanded-port)
	    (flush-output expanded-port)
	    expanded))))
    #f))
