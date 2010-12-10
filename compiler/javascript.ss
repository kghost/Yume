(use-modules (srfi srfi-1))
(use-modules (ice-9 rw))

(define write-string write-string/partial)

(define compile
  (lambda (output p)
    (letrec ((compile-quote
	       (lambda (p)
		 (cond
		   ((pair? p) (write-string "yume.cons(" output)
			      (compile-quote (car p))
			      (compile-quote (cdr p)))
		   ((eq? p '()) (write-string "yume._null_list") output)
		   ((symbol? p) (write-string "new yume._symbol(\"" output)
				(write-string (symbol->string p) output)
				(write-string "\")" output))
		   ((boolean? p) (write-string (if p
						 "yume._boolean._true"
						 "yume._boolean._false")))
		   ((number? p) (write-string "new yume._number(" output)
				(write p)
				(write-string ")" output))
		   ((char? p) (write-string "new yume._char(\"" output)
			      (write-char p)
			      (write-string "\")" output))
		   ((string? p) (write-string "new yume._string(\"" output)
				(write-string p output)
				(write-string "\")" output))
		   ((vector? p) (raise "TODO"))
		   (else (raise (list "internal-compile-error" "unknown quote data" p))))))


	     (tokens
	       `((yume:label
		   ,(lambda (p)
		      (compile (cadr p))))

		 (yume:quote
		   ,(lambda (p)
		      (lambda ()
			(compile-quote (cadr p)))))

		 (yume:cons
		   ,(lambda (p)
		      (let ((a (compile (cadr p)))
			    (d (compile (caddr p))))
			(lambda ()
			  (write-string "yume.cons(" output)
			  (a)
			  (write-string ", " output)
			  (d)
			  (write-char #\) output)))))

		 (yume:global-get
		   ,(lambda (p)
		      (lambda ()
			(write-string "yume.global_get(\"" output)
			(write-string (symbol->string (cadr p)))
			(write-string "\")" output))))

		 (yume:procedure-new
		   ,(lambda (p)
		      (let ((fun (compile (cadr p)))
			    (scope (compile (caddr p))))
			(lambda ()
			  (write-string "new yume._procedure(" output)
			  (fun)
			  (write-string ", " output)
			  (scope)
			  (write-string ", " output)
			  (write (cadddr p))
			  (write-string ", " output)
			  (if (car (cddddr p))
			    (write-string "true" output)
			    (write-string "false" output))
			  (write-string ")" output)))))

		 (yume:procedure-call
		   ,(lambda (p)
		      (let ((fun (compile (cadr p)))
			    (cps (compile (caddr p)))
			    (args (compile (cadddr p))))
			(lambda ()
			  (write-string "yume.procedure_call(" output)
			  (fun)
			  (write-string ", " output)
			  (cps)
			  (write-string ", " output)
			  (args)
			  (write-string ")" output)))))

		 (yume:lambda-cps
		   ,(lambda (p)
		      (let ((name (cadr p)) (fun (compile (cadddr p))))
			(write-string "var P_" output)
			(write-string name output)
			(write-string " = function (cps, scope) {" output)
			(newline output)
			(write-string "return " output)
			(fun)
			(write-char #\; output)
			(newline output)
			(write-string "};" output)
			(newline output)
			(lambda ()
			  (write-string "P_" output)
			  (write-string name output)))))

		 (yume:lambda-continue
		   ,(lambda (p)
		      (if (null-list? (cdddr p))
			(compile (caddr p))
			(raise (list "internal-compile-error" p "contains multiple statements")))))

		   ,(lambda (p)
		      (let ((name (cadr p)) (fun (compile (cadddr p))))
			(write-string "var C_" output)
			(write-string name output)
			(write-string " = function (cps, scope) {" output)
			(newline output)
			(write-string "return " output)
			(fun)
			(write-char #\; output)
			(newline output)
			(write-string "};" output)
			(newline output)
			(lambda ()
			  (write-string "P_" output)
			  (write-string name output)))))
		 ))

	     (compile
	       (lambda (p)
		 (cond ((pair? p)
			(let ((token (assq (car p) tokens)))
			  (if token
			    ((cadr token) p)
			    (let ((fun (compile (car p)))
				  (params (reverse (fold
						     (lambda (p tail)
						       (cons
							 (compile p)
							 tail))
						     '()
						     (cdr p)))))
			      (lambda ()
				(fun)
				(write-char #\( output)
				(pair-fold
				  (lambda (param tail)
				    (or (eq? param params) (write-string ", " output))
				    ((car param)))
				  '()
				  params)
				(write-string ")" output))))))
		       (else
			 (lambda ()
			   (cond
			     ((symbol? p) (write-string (symbol->string p) output))
			     ((boolean? p) (write-string (if p
							   "yume._boolean._true"
							   "yume._boolean._false")))
			     ((number? p) (write-string "new yume._number(" output)
					  (write p)
					  (write-string ")" output))
			     ((char? p) (write-string "new yume._char(\"" output)
					(write-char p)
					(write-string "\")" output))
			     ((string? p) (write-string "new yume._string(\"" output)
					  (write-string p output)
					  (write-string "\")" output))
			     ((vector? p) (raise "TODO"))
			     (else (raise (list "internal-compile-error" "unknown data" p))))))))))

      (write-string "var " output)
      (if (pair? (cdr (command-line)))
	(write-string (cadr (command-line)) output)
	(write-string "unamed_module" output))
      (write-string " = function() {" output)
      (newline output)
      (let ((ret (compile p)))
	(write-string "return ")
	(ret)
	(write-string ";")
	(newline output)
	(write-string "};")
	(newline output)
	))))
