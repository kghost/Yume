(expand '(
	  (define-syntax let
	    (syntax-rules ()
			  ((let ((var expr) ...) body ...)
			   ((lambda (var ...) body ...) expr ...))))

	  (let ((a 1)(b 2)) (+ b a))
	  ))

(expand '(
	  (define-syntax foo
	    (syntax-rules ()
			  ((foo (a ...) (b ...) ...) '(((a b) ...) ...))))
	  (foo (1 2) (3 4) (5 6 7))
	  ))

(expand '(
	  (define-syntax let
	    (syntax-rules ()
			  ((let ((var expr) ...) body ...)
			   ((lambda (var ...) body ...) expr ...))))

	  (define-syntax letrec
	    (syntax-rules ()
			  ((_ ((var init) ...) . body)
			   (let ((var 'undefined) ...)
			     (let ((var (let ((temp init)) (lambda () (set! var temp))))
				   ...
				   (bod (lambda () . body)))
			       (var) ... (bod))))))

	  (letrec ((a 10) (b 20)) (+ a b))
	  ))

(expand '(
	  (define-syntax rec
	    (syntax-rules ()
			  ((rec (a ...) ...) (xxx (... ...) (a ...) ...))))

	  (rec (1 2 3) (4 5 6 7 8) (9 10))
	  ))

(expand '(
	  (define-syntax reverse-order
	    (syntax-rules ()
			  ((_ e) (reverse-order e ()))
			  ((_ (e . rest) r) (reverse-order rest (e . r)))
			  ((_ () r) r)))
	  (reverse-order (2 3 -))
	  ))

(expand '(
	  (define-syntax foo
	    (syntax-rules ()
			  ((foo (x ...)) 1)
			  ((foo (x . y)) 2)))
	  (foo (1 2 3))     ;=> 1
	  (foo (1 . (2 3))) ;=> 1
	  (foo (1 . 2))     ;=> 2
	  ))

(expand '(
	  (define-syntax def-multi
	    (syntax-rules ()
			  ((def-multi (var ...) expr ...)
			   (begin
			     (define var expr)
			     ...))))
	  (def-multi (a b) (+ 1 2) (+ 3 4))
	  (+ a b) ;=> 10
	  ))

(expand '(
	  (define-syntax define-values
	    (syntax-rules ()
			  ((define-values () exp)
			   (call-with-values (lambda () exp) (lambda () 'unspecified)))
			  ((define-values (var . vars) exp)
			   (begin
			     (define var (call-with-values (lambda () exp) list))
			     (define-values vars (apply values (cdr var)))
			     (define var (car var))))
			  ((define-values var exp)
			   (define var (call-with-values (lambda () exp) list)))))

	  (define-values (one two) (values 1 2))
	  ))

