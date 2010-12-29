(define (macros program)
  (call-with-values
    (lambda () (partition (lambda (e) (and (pair? e) (eq? (car e) 'define-syntax))) program))
    (lambda (inline-macros program)
      (call-with-values
	(lambda () (partition (lambda (e) (and (pair? e) (eq? (car e) 'use-syntax))) program))
	(lambda (use-syntax program)
	  (values
	    (append
	      inline-macros
	      (apply
		append
		(map
		  (lambda (s)
		    (interpret
		      (open-input-file
			(string-append "library/"
				       (apply string-append
					      (fold-right
						(lambda (e t)
						  (cons (symbol->string e)
							(if t (cons "/" t) '(".scm"))))
						#f
						(cadr s)))))))
		  use-syntax)))
	    program))))))

