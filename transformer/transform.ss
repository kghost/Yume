(use-modules (srfi srfi-1))

(define transform
  (lambda (p)
    (letrec
      ((debug-filter
	 (lambda (p)
	   (dotted-map (lambda (e)
		  (if (pair? e)
		    (dotted-map (lambda (e)
			   (if (pair? e)
			     (dotted-map (lambda (e)
				    (if (pair? e)
				      (dotted-map (lambda (e)
					     (if (pair? e)
					       '...
					       e))
					   e)
				      e))
				  e)
			     e))
			 e)
		    e))
		p)))

       (transform-begin
	 (lambda (out name index env expressions)
	   (let ((expr-name (string-append name (number->string index) "_e")))
	     (if (pair? expressions)
	       (out
		 identity
		 (transform-begin
		   (lambda (return result)
		     (return
		       (cons
			 (transform
			   (lambda (return result)
			     (return result))
			   expr-name env (car expressions))
			 result)))
		   name (+ index 1) env (cdr expressions)))
	       (if (eq? '() expressions)
		 (out identity '())
		 (raise (list "transform-error" "begin is not list" expressions)))))))

       (transform-quote
	 (lambda (out name env expression)
	   (if (and (pair? expression) (null-list? (cdr expression)))
	     (out identity `(yume:quote ,name ,(car expression)))
	     (raise (list "transform-error" "quote" expression)))))

       (transform-define
	 (lambda (out name env define)
	   (let ((variable (car define)) (value (cadr define)) (null (cddr define)))
	     (if (and (symbol? variable) (null-list? null))
	       (out
		 identity
		 (transform
		   (lambda (return result)
		     (return
		       `(yume:global-add
			  ,variable
			  ,retult)))
		   (string-append name "_d") env value))
	       (raise (list "transform-error" "define" define))))))

       (transform-instant
	 (lambda (out name env expression)
	   (out identity ,expression)))

       (transform-lambda
	 (lambda (out name env params)
	   (let ((args (car params)) (expressions (cadr params)) (null (cddr params)))
	     (if (null? null)
	       (if (dotted-every symbol? args)
		 (out
		   identity
		   (transform
		     (lambda (return result)
		       `(yume:procedure-new
			  (yume:label
			    (yume:lambda-cps
			      ,name
			      (cps scope)
			      ,result)
			    ,(debug-filter expressions))
			  scope ,(dotted-length args) ,(dotted-list? args)))
		     (string-append name "_f") (cons args env) expressions))
		 (raise (list "transform-error" "lambda args error" args)))
	       (raise (list "internal-transform-error" "lambda contain multiple statements"))))))

       (transform-params
	 (lambda (out name index env params)
	   (if (pair? params)
	     (out
	       identity
	       `(yume:cons
		  (transform p (string-append name (number->string index) "_e") env (car params))
		  (transform-params p name (+ index 1) env (cdr params))))
	     (if (null-list? params)
	       (out identity 'yume:null-list)
	       (raise "transform-error" "call params is not list")))))

       (transform-call
	 (lambda (out name env params)
	   `(
		       (transform (string-append name "_call") env (car params))
		       (transform-params (string-append name "_p") 0 env (cdr params))
	   `(yume:label
		  (yume:lambda-cps
		    ,name
		    (cps scope)
		    ,(do-transform
		       (lambda (result-fun)
			 (do-transform
			   (lambda (result)
			     `(yume:procedure-call ,result-fun cps ,result))
			   (lambda (result)
			     `(,result
				(yume:continue-new
				  (yume:lambda-continue
				    ,name
				    (cps scope result)
				    (yume:procedure-call ,result-fun cps result))
				  cps scope)
				scope))))
		       (lambda (result)
			 `(,result
			    ,(do-transform
			       (transform-params (string-append name "_p") 0 env (cdr params))
			       (lambda (result)
				 `(yume:continue-new
				    (yume:lambda-continue
				      ,name
				      (cps scope result)
				      (yume:procedure-call result cps ,result))
				    cps scope))
			       (lambda (result)
				 `(yume:continue-new
				    (yume:lambda-continue
				      ,(string-append name "_1")
				      (cps scope result)
				      (,result
					(yume:continue-new
					  (yume:lambda-continue
					    ,(string-append name "_2")
					    (cps scope result)
					    ; borrow scope to pass procedure being called
					    (yume:procedure-call scope cps result))
					  cps result)
					scope))
				    cps scope)))
			    scope))))
		  ,(debug-filter params))))

       (dotted-length
	 (lambda (lis)
	   (if (pair? lis)
	     (+ 1 (dotted-length (cdr lis)))
	     0)))

       (dotted-map
	 (lambda (proc lis)
	   (if (pair? lis)
	     (cons (proc (car lis)) (dotted-map proc (cdr lis)))
	     (proc lis))))

       (dotted-every
	 (lambda (f lis)
	   (if (pair? lis)
	     (and (f (car lis)) (dotted-every f (cdr lis)))
	     (or (eq? '() lis) (f lis)))))

       (dotted-fold
	 (lambda (kons kdot knil lis)
	   (if (pair? lis)
	     (dotted-fold kons kdot (kons (car lis) knil) (cdr lis))
	     (kdot lis knil))))

       (dotted-pair-fold
	 (lambda (kons kdot knil lis)
	   (if (pair? lis)
	     (dotted-pair-fold kons kdot (kons lis knil) (cdr lis))
	     (kdot lis knil))))

       (transform-variable
	 (lambda (name env variable)
	   (call/cc
	     (lambda (return)
	       (fold
		 (lambda (frame tail)
		   (dotted-fold
		     (lambda (var tail)
		       (if (eq? var variable)
			 (return `(#t (yume:car ,tail)))
			 (list 'yume:cdr tail)))
		     (lambda (var tail)
		       (and (eq? var variable) (return (list #t tail))))
		     (list 'yume:car tail) frame)
		   (list 'yume:cdr tail))
		 'scope env)
	       `(#t (yume:global-get ,variable))))))

       (transform-set!
	 (lambda (name env args)
	   (let ((variable (car args)) (value (cadr args)) (null (cddr args)))
	     (if (eq? '() null)
	       (let ((r (lambda (result)
			  (call/cc
			    (lambda (return)
			      (fold
				(lambda (frame tail)
				  (if (pair? frame)
				    (dotted-pair-fold
				      (lambda (lis tail)
					(cond ((eq? (car lis) variable)
					       (return `(yume:set-car! ,tail ,result)))
					      ((eq? (cdr lis) variable)
					       (return `(yume:set-cdr! ,tail ,result)))
					      (else (list 'yume:cdr tail))))
				      (lambda x x)
				      (list 'yume:car tail) frame)
				    (and (eq? frame variable) (return `(yume:set-car! ,tail ,result))))
				  (list 'yume:cdr tail))
				'scope env)
			      `(yume:global-set ,variable ,result))))))
		 `(#f (yume:label
			(yume:lambda-cps
			  ,name
			  (cps scope)
			  ,(do-transform
			     (transform (string-append name "_set") env value)
			     (lambda (result) `(yume:continue-call cps ,(r result)))
			     (lambda (result)
			       `(,result
				  (yume:continue-new
				    (yume:lambda-continue
				      ,name
				      (cps scope result)
				      (yume:continue-call cps ,(r 'result)))
				    cps scope)
				  scope))))
			,(debug-filter value))))
	       (raise (list "transform-error" "syntax error: set!" args))))))

       (transform-if
	 (lambda (name env args)
	   (let ((test (car args)) (true (cadr args)) (false (caddr args)) (null (cdddr args)))
	     (if (eq? '() null)
	       (do-transform
		 (transform (string-append name "_test") env test)
		 (lambda (result)
		   `(#f (yume:label
			  (yume:lambda-cps
			    ,name
			    (cps scope)
			    (yume:if
			      ,result
			      ,(do-transform
				 (transform (string-append name "_true") env true)
				 (lambda (result)
				   `(yume:continue-call cps ,result))
				 (lambda (result)
				   `(,result cps scope)))
			      ,(do-transform
				 (transform (string-append name "_false") env false)
				 (lambda (result)
				   `(yume:continue-call cps ,result))
				 (lambda (result)
				   `(,result cps scope)))))
			  ,(debug-filter test))))
		 (lambda (result)
		   `(#f (yume:label
			  (yume:lambda-cps
			    ,name
			    (cps scope)
			    (,result
			      (yume:continue-new
				(yume:lambda-continue
				  ,name
				  (cps scope result)
				  (yume:if result
					   ,(do-transform
					      (transform (string-append name "_true") env true)
					      (lambda (result)
						`(yume:continue-call cps ,result))
					      (lambda (result)
						`(,result cps scope)))
					   ,(do-transform
					      (transform (string-append name "_false") env false)
					      (lambda (result)
						`(yume:continue-call cps ,result))
					      (lambda (result)
						`(,result cps scope)))))
				cps scope)
			      scope))
			  ,(debug-filter test)))))
	       (raise (list "transform-error" "syntax error: if" args))))))

       (transform
	 (lambda (name env expression)
	   (cond ((pair? expression)
		  (let ((op (car expression))(args (cdr expression)))
		    (cond ((eq? op 'quote) (transform-quote name env args))
			  ((eq? op '$define) (transform-define name env args))
			  ((eq? op '$lambda) (transform-lambda name env args))
			  ((eq? op '$begin) (transform-begin name 0 env args))
			  ((eq? op '$if) (transform-if name env args))
			  ((eq? op '$set!) (transform-set! name env args))
			  (else (transform-call name env expression)))))
		 ((symbol? expression) (transform-variable name env expression))
		 (else (transform-instant name env expression)))))
       )

      (cadr (transform "g" '() p)))))
