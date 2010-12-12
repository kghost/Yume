(use-modules (srfi srfi-1))
(use-modules (ice-9 rw))
(use-modules (ice-9 pretty-print))

(define write-string write-string/partial)

(define compile
  (lambda (output p module)
    (letrec ((escape
	       (lambda (s)
		 (list->string
		   (fold-right
		     (lambda (c tail)
		       (if (or (eqv? c #\\) (eqv? c #\') (eqv? c #\"))
			 (cons #\\ (cons c tail))
			 (cons c tail)))
		     '()
		     (string->list s)))))

	     (compile-quote
	       (lambda (p)
		 (cond
		   ((pair? p) (write-string "yume.cons(" output)
			      (compile-quote (car p))
			      (write-string ", " output)
			      (compile-quote (cdr p))
			      (write-string ")" output))
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
		   ((char? p) (write-string "new yume._char(" output)
			      (write (char->integer p) output)
			      (write-string ")" output))
		   ((string? p) (write-string "new yume._string(\"" output)
				(write-string (escape p) output)
				(write-string "\")" output))
		   ((vector? p) (raise "TODO"))
		   (else (raise (list "internal-compile-error" "unknown quote data" p))))))

	     (tokens
	       `((yume:label
		   ,(lambda (p)
		      (let ((r (compile (cadr p))))
			(pretty-print (caddr p)
			  output #:per-line-prefix "// ")
			r)))

		 (yume:quote
		   ,(lambda (p)
		      (let ((name (cadr p)))
			(write-string "var Q_" output)
			(write-string name output)
			(write-string " = " output)
			(compile-quote (caddr p))
			(write-string ";" output)
			(newline output)
			(lambda ()
			  (write-string "Q_" output)
			  (write-string name output)))))

		 (yume:null-list
		   ,(lambda (p)
		      (lambda ()
			(write-string "yume._null_list" output))))

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

		 (yume:car
		   ,(lambda (p)
		      (let ((s (compile (cadr p))))
			(lambda ()
			  (write-string "yume.car(" output)
			  (s)
			  (write-char #\) output)))))

		 (yume:cdr
		   ,(lambda (p)
		      (let ((s (compile (cadr p))))
			(lambda ()
			  (write-string "yume.cdr(" output)
			  (s)
			  (write-char #\) output)))))

		 (yume:set-car!
		   ,(lambda (p)
		      (let ((variable (compile (cadr p)))
			    (value (compile (caddr p))))
			(lambda ()
			  (write-string "yume.set_car_bang(" output)
			  (variable)
			  (write-string ", " output)
			  (value)
			  (write-char #\) output)))))

		 (yume:set-cdr!
		   ,(lambda (p)
		      (let ((variable (compile (cadr p)))
			    (value (compile (caddr p))))
			(lambda ()
			  (write-string "yume.set_cdr_bang(" output)
			  (variable)
			  (write-string "\", " output)
			  (value)
			  (write-char #\) output)))))

		 (yume:if
		   ,(lambda (p)
		      (let ((test (compile (cadr p)))
			    (true (compile (caddr p)))
			    (false (compile (cadddr p))))
			(lambda ()
			  (write-string "yume.test(" output)
			  (test)
			  (write-string ") ? " output)
			  (true)
			  (write-string " : " output)
			  (false)))))

		 (yume:global-add
		   ,(lambda (p)
		      (let ((value (compile (caddr p))))
			(lambda ()
			  (write-string "yume.global_add(\"" output)
			  (write-string (symbol->string (cadr p)) output)
			  (write-string "\", " output)
			  (value)
			  (write-string ")" output)))))

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

		 (yume:continue-new
		   ,(lambda (p)
		      (let ((fun (compile (cadr p)))
			    (cps (compile (caddr p)))
			    (scope (compile (cadddr p))))
			(lambda ()
			  (write-string "new yume._continue(" output)
			  (fun)
			  (write-string ", " output)
			  (cps)
			  (write-string ", " output)
			  (scope)
			  (write-string ")" output)))))

		 (yume:continue-call
		   ,(lambda (p)
		      (let ((cps (compile (cadr p)))
			    (result (compile (caddr p))))
			(lambda ()
			  (write-string "yume.continue_call(" output)
			  (cps)
			  (write-string ", " output)
			  (result)
			  (write-string ")" output)))))

		 (yume:lambda-continue
		   ,(lambda (p)
		      (let ((name (cadr p)) (fun (compile (cadddr p))))
			(write-string "var C_" output)
			(write-string name output)
			(write-string " = function (cps, scope, result) {" output)
			(newline output)
			(write-string "return " output)
			(fun)
			(write-char #\; output)
			(newline output)
			(write-string "};" output)
			(newline output)
			(lambda ()
			  (write-string "C_" output)
			  (write-string name output)))))))

	     (compile
	       (lambda (p)
		 (cond ((pair? p)
			(let ((token (assq (car p) tokens)))
			  (if token
			    ((cadr token) p)
			    ; TODO: check transtormer bug, inline fun call must take 2 arguments
			    (begin
			      (and (null-list? (cddr p)) (raise p))
			      (or (null-list? (cdddr p)) (raise p))
			      (let ((fun (compile (car p)))
				    (p1 (compile (cadr p)))
				    (p2 (compile (caddr p))))
				(lambda ()
				  (fun)
				  (write-char #\( output)
				  (p1)
				  (write-string ", " output)
				  (p2)
				  (write-string ")" output)))))))
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
			     ((char? p) (write-string "new yume._char(" output)
					(write (char->integer p) output)
					(write-string ")" output))
			     ((string? p) (write-string "new yume._string(\"" output)
					  (write-string (escape p) output)
					  (write-string "\")" output))
			     ((vector? p) (raise "TODO"))
			     (else (raise (list "internal-compile-error" "unknown data" p))))))))))

      (if module
	(begin
	  (write-string "var " output)
	  (write-string module output)
	  (write-string " = function() {" output)
	  (newline output)
	  (let ((ret (compile p)))
	    (write-string "return ")
	    (ret)
	    (write-string ";" output)
	    (newline output)
	    (write-string "};" output)
	    (newline output)))

	(begin
	  (write-string "(function() {" output)
	  (newline output)
	  (let ((ret (compile p)))
	    (write-string "return " output)
	    (ret)
	    (write-string ";" output)
	    (newline output)
	    (write-string "})();" output)
	    (newline output)))))))

