(use-modules (srfi srfi-1))

(define transform
  (lambda (p)
    (letrec
      ((do-transform
	 (lambda (result inline non-inline)
	   (if (car result)
	     (inline (cadr result))
	     (non-inline (cadr result)))))

       (debug-filter
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
	 (lambda (name index env expressions)
	   (let ((expr-name (string-append name (number->string index) "_e")))
	     (if (pair? expressions)
	       (if (eq? '() (cdr expressions))
		 (transform expr-name env (car expressions))
		 (do-transform
		   (transform expr-name env (car expressions))
		   (lambda (result)
		     (transform-begin name (+ index 1) env (cdr expressions)))
		   (lambda (result)
		     `(#f (yume:label
			    (yume:lambda-cps
			      ,(string-append name (number->string index))
			      (cps scope)
			      (,result
				(yume:continue-new
				  ,(do-transform
				     (transform-begin name (+ index 1) env (cdr expressions))
				     (lambda (result)
				       `(yume:lambda-continue
					  ,(string-append name (number->string index))
					  (cps scope result)
					  (yume:continue-call cps ,result)))
				     (lambda (result)
				       `(yume:lambda-continue
					  ,(string-append name (number->string index))
					  (cps scope result)
					  (,result cps scope))))
				  cps scope)
				scope))
			    ,(debug-filter (car expressions)))))))
	       (raise (list "transform-error" "begin is not list" expressions))))))

       (transform-quote
	 (lambda (name env expression)
	   (if (and (pair? expression) (null-list? (cdr expression)))
	     `(#t (yume:quote ,name ,(car expression)))
	     (raise (list "transform-error" "quote" expression)))))

       (transform-define
	 (lambda (name env define)
	   (let ((variable (car define)) (value (cadr define)) (null (cddr define)))
	     (if (null-list? null)
	       `(#f (yume:label
		      (yume:lambda-cps
			,name
			(cps scope)
			,(do-transform
			   (transform (string-append name "_e") env value)
			   (lambda (result)
			     `(yume:continue-call cps (yume:global-add ,variable ,result)))
			   (lambda (result)
			     `(,result
				(yume:continue-new
				  (yume:lambda-continue
				    ,name
				    (cps scope result)
				    (yume:continue-call cps (yume:global-add ,variable result)))
				  cps #f)
				scope))))
		      ,(debug-filter value)))
	       (raise (list "transform-error" "define" define))))))

       (transform-instant
	 (lambda (name env expression)
	   `(#t ,expression)))

       (transform-lambda
	 (lambda (name env params)
	   (let ((args (car params)) (expressions (cdr params)))
	     (if (dotted-every symbol? args)
	       `(#t (yume:procedure-new
		      ,(do-transform
			 (transform-begin (string-append name "_f_s") 0 (cons args env) expressions)
			 (lambda (result)
			   `(yume:label
			      (yume:lambda-cps
				,name
				(cps scope)
				(yume:continue-call cps ,result))
			      ,(debug-filter expressions)))
			 (lambda (result) result))
		      scope ,(dotted-length args) ,(dotted-list? args)))
	       (raise (list "transform-error" "lambda args error" args))))))

       (transform-params
	 (lambda (name index env params)
	   (if (pair? params)
	     (do-transform
	       (transform-params name (+ index 1) env (cdr params))
	       (lambda (result)
		 (do-transform
		   (transform (string-append name (number->string index) "_e") env (car params))
		   (lambda (result-e)
		     `(#t (yume:cons ,result-e ,result)))
		   (lambda (result-e)
		     `(#f (yume:label
			    (yume:lambda-cps
			      ,(string-append name (number->string index))
			      (cps scope)
			      (,result-e
				(yume:continue-new
				  (yume:lambda-continue
				    ,(string-append name (number->string index))
				    (cps scope result)
				    (yume:continue-call cps (yume:cons result ,result)))
				  cps scope)
				scope))
			    ,(debug-filter (car params)))))))
	       (lambda (result)
		 `(#f (yume:label
			(yume:lambda-cps
			  ,(string-append name (number->string index))
			  (cps scope)
			  (,result
			    (yume:continue-new
			      ,(do-transform
				 (transform (string-append name (number->string index) "_e") env (car params))
				 (lambda (result-e)
				   `(yume:lambda-continue
				      ,(string-append name (number->string index))
				      (cps scope result)
				      (yume:continue-call cps (yume:cons ,result-e result)))
				   )
				 (lambda (result-e)
				   `(yume:lambda-continue
				      ,(string-append name (number->string index) "_1")
				      (cps scope result)
				      (,result-e
					(yume:continue-new
					  (yume:lambda-continue
					    ,(string-append name (number->string index) "_2")
					    (cps scope result)
					    ; borrow scope to pass result
					    (yume:continue-call cps (yume:cons result scope)))
					  cps result)
					scope))))
			      cps scope)
			    scope))
			,(debug-filter (cdr params))))))
	     (if (null-list? params)
	       '(#t (yume:null-list))
	       (raise "transform-error" "call params is not list")))))

       (transform-call
	 (lambda (name env params)
	   `(#f (yume:label
		  (yume:lambda-cps
		    ,name
		    (cps scope)
		    ,(do-transform
		       (transform (string-append name "_call") env (car params))
		       (lambda (result-fun)
			 (do-transform
			   (transform-params (string-append name "_p") 0 env (cdr params))
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
		  ,(debug-filter params)))))

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
		    (cond ((eq? op '$quote) (transform-quote name env args))
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
