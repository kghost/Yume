(use-modules (srfi srfi-1))
(use-modules (srfi srfi-9))
(use-syntax (ice-9 syncase))
(use-modules (ice-9 rw))
(use-modules (ice-9 pretty-print))

(load "../../library/match.scm")
(load "../../src/misc.ss")
(load "../../src/reader.ss")
(load "../../src/syntax.ss")
(load "../../src/expand.ss")
(load "../../src/transform.ss")
(load "javascript.ss")

(define build-in-macros (interpret (open-input-file "library/build-in-macros.ss")))
(define build-in-syntax (interpret (open-input-file "library/build-in-syntax.ss")))
(define feature-use-modules (interpret (open-input-file "library/feature-use-modules.ss")))
(define program (interpret (current-input-port)))

(debug-set! stack 2000000)

(let ((output (current-output-port)))
  (compile output
	   (transform
	     (call-with-values
	       (lambda () (macros program))
	       (lambda (inline-macros statements)
		 (expand
		   (append
		     build-in-syntax
		     build-in-macros
		     feature-use-modules
		     inline-macros)
		   (cons 'begin statements)))))
	   (and (pair? (cdr (command-line)))
		(cadr (command-line))))
  (newline output))

