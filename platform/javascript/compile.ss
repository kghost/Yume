(use-modules (srfi srfi-1))

(load "../../src/reader.ss")
(load "../../src/expand.ss")
(load "../../src/transform.ss")
(load "../../src/javascript.ss")

(define build-in-macros (interpret (open-input-file "library/build-in-macros.ss")))
(define build-in-syntax (interpret (open-input-file "library/build-in-syntax.ss")))
(define feature-use-modules (interpret (open-input-file "library/feature-use-modules.ss")))
(define program (interpret (current-input-port)))

(debug-set! stack 200000)

(compile (current-output-port)
	 (transform
	   (expand
	     (cons 'begin
		   (append
		     build-in-syntax
		     build-in-macros
		     feature-use-modules
		     program))))
	 (and (pair? (cdr (command-line)))
	      (cadr (command-line))))
(newline)

