(define (equal? this that)
  (cond ((pair? this) (and (pair? that)
			   (equal? (car this) (car that))
			   (equal? (cdr this) (cdr that))))
	(else (eqv? this that))))

(letrec ((dotted-pair-for-each
	   (lambda (proc proc-tail lis)
	     (if (pair? lis)
	       (begin
		 (proc lis)
		 (dotted-pair-for-each proc proc-tail (cdr lis)))
	       (proc-tail lis)))))

  (define (write obj . port)
    (let ((o (or (and (not (null-list? port)) (car port)) (current-output-port))))
      (cond ((pair? obj) (write-char #\( o)
			 (dotted-pair-for-each
			   (lambda (l)
			     (write (car l) o)
			     (and (pair? (cdr l)) (write-char #\space o)))
			   (lambda (t)
			     (or (null? t)
				 (begin (write-string " . " o)
					(write t o)))
			     (write-char #\) o))
			   obj))
	    ((symbol? obj) (write-string (symbol->string obj) o))
	    ((boolean? obj) (if obj
			      (write-string "#t" o)
			      (write-string "#f" o)))
	    ((number? obj) (write-string (number->string obj) o))
	    ((string? obj) (write-string "\"" o)
			   (write-string (escape obj) o)
			   (write-string "\"" o))
	    ((char? obj) (write-string "#\\" o) (write-char obj o))
	    ((null? obj) (write-string "()" o))
	    (else (write-string "<" o)
		  (write-string (type obj) o)
		  (write-string ">" o))))))

(define (null? obj)
  (eq? obj '()))

(define (not obj)
  (if obj #f #t))

(define (boolean? obj)
  (or (eq? obj #t) (eq? obj #f)))

(define (string . char)
  (list->string char))

