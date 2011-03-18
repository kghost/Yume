(define-record-type
  :hygienic-ident
  (hygienic-ident symbol subst mark)
  hygienic-ident?
  (symbol hygienic-ident-symbol)
  (subst hygienic-ident-subst)
  (mark hygienic-ident-mark))

(define expand
  (lambda (macros p)
    (letrec ((assign-name (lambda (origin) (gensym (string-append (symbol->string origin) "|"))))

	     ; hygienic macro see http://www.cs.indiana.edu/~dyb/pubs/LaSC-5-4-pp295-326.pdf
	     ; r: env
	     ; s: symbol
	     ; e: expr
	     ; p: input expr
	     ; i: ident
	     ; m: mark
	     (hygienic-ident-eq?
	       (lambda (i1 i2)
		 (and (eqv? (hygienic-ident-symbol i1) (hygienic-ident-symbol i2))
		      (eqv? (hygienic-ident-subst i1) (hygienic-ident-subst i2))
		      (lset= eqv? (hygienic-ident-mark i1) (hygienic-ident-mark i2)))))
	     (hygienic-mark-init
	       (lambda (e)
		 (let ((mark (list (assign-name 'init))))
		   (let r ((e e))
		     (cond ((symbol? e) (hygienic-ident e e mark))
			   ((pair? e) (dotted-map (lambda (e) (r e)) e))
			   (else e))))))
	     (hygienic-mark
	       (lambda (e m)
		 (cond ((hygienic-ident? e) (hygienic-ident
					      (hygienic-ident-symbol e)
					      (hygienic-ident-subst e)
					      (lset-xor eqv? (hygienic-ident-mark e) (list m))))
		       ((pair? e)
			(dotted-map
			  (lambda (e)
			    (hygienic-mark e m))
			  e))
		       (else e))))
	     (hygienic-subst
	       (lambda (e i s)
		 (let ((n (hygienic-ident
			    (hygienic-ident-symbol i)
			    s
			    (hygienic-ident-mark i))))
		   (cond ((hygienic-ident? e) (if (hygienic-ident-eq? e i) n e))
			 ((pair? e)
			  (dotted-map
			    (lambda (e)
			      (hygienic-subst e i s))
			    e))
			 (else e)))))
	     (hygienic-resove
	       (lambda (i)
		 (hygienic-ident-subst i)))

	     (prepare-rules
	       (lambda (macro)
		 (match macro
			(((? hygienic-ident? name)
			  ((? (lambda (s) (and (hygienic-ident? s) (eqv? (hygienic-resove s) 'syntax-rules))) x)
			   (? (lambda (e) (and (proper-list? e) (every hygienic-ident? e))) auxiliary-keywords)
			   (? (lambda (rule)
				(match rule
				       ((matching (? (lambda (p)
						       (let check-dddot-at-head ((p p))
							 (or (not (pair? p))
							     (and (not (eq? (car p) '...))
								  (dotted-every check-dddot-at-head (car p))
								  (dotted-every check-dddot-at-head (cdr p))))))
						     expr)) #t)
				       (_ (raise (list "expand-error" "macro-syntax-error" "... not at end" macro)))))
			      rules) ___))
			 (list (hygienic-resove name) (cons (map hygienic-ident-symbol auxiliary-keywords) rules)))
			(_ (raise (list "expand-error" "macro-syntax-error" macro))))))

	     (match-syntax-rules
	       ; -> (syntax-expr renames . bindings)
	       ; bindings => ((variable . binding) ...)
	       ; binding => (#t . (binding ...)) if is recursive or (#f . bind)
	       ; renames => (symbol ...)
	       (lambda (auxiliary-keywords rule-set p)
		 (letrec ((match-rec-empty-binding
			    (lambda (pre rule)
			      (cond ((pair? rule) (match-rec-empty-binding (match-rec-empty-binding pre (cdr rule)) (car rule)))
				    ((vector? rule) (match-rec-empty-binding pre (vector->list rule)))
				    ((and (hygienic-ident? rule) (not (find (eq-this? (hygienic-ident-symbol rule)) auxiliary-keywords))) (cons (list rule) pre))
				    (else pre))))

			  (match-rec
			    (lambda (pre rule p)
			      ; pre => (renames . bindings)
			      (cond ((pair? p) (let ((next (match rule (car p))))
						 (and next (match-rec
							     (cons
							       (append (car next) (car pre))
							       (fold-right
								 (lambda (p n tail) (cons (cons (car p) (cons n (cdr p))) tail))
								 '() (cdr pre) (map cadr (cdr next))))
							     rule (cdr p)))))
				    ((null? p) (cons (car pre) (map (lambda (e) (list (car e) (cons #t (cdr e)))) (cdr pre))))
				    (else #f))))

			  (match ; -> (renames . bindings) or #f if not match
			    (lambda (rule p)
			      (cond ((hygienic-ident? rule)
				     (if (find (eq-this? (hygienic-ident-symbol rule)) auxiliary-keywords)
				       (if (and (hygienic-ident? p) (eqv? (hygienic-ident-symbol rule) (hygienic-ident-symbol p)))
					 (cons '() '())
					 #f)
				       (if (eqv? (string-ref (symbol->string (hygienic-ident-symbol rule)) 0) #\|)
					 (and (hygienic-ident? p)
					      (cons (list p) (list (list rule (cons #f p)))))
					 (cons '() (list (list rule (cons #f p)))))))
				    ((pair? rule) (if (and (pair? (cdr rule)) (hygienic-ident? (cadr rule)) (null? (cddr rule)) (eqv? '... (hygienic-ident-symbol (cadr rule))))
						    ; XXX: seems '... can only appear at the end of list, or need a fsm processer :(
						    (match-rec (cons '() (match-rec-empty-binding '() (car rule))) (car rule) p)
						    (and (pair? p)
							 (let ((head (match (car rule) (car p))) (tail (match (cdr rule) (cdr p))))
							   (and (and head tail)
								(cons (append (car head) (car tail)) (append (cdr head) (cdr tail))))))))
				    ((and (vector? rule) (vector? p)) (match (vector->list rule) (vector->list p)))
				    (else (and (equal? rule p) (cons '() '())))))))
		   (if (null-list? rule-set)
		     (raise (list "expand-error" "syntax-error" p "do not match ()"))
		     (let ((result (match (caar rule-set) p)))
		       (if result
			 (cons (cadar rule-set) result)
			 (match-syntax-rules auxiliary-keywords (cdr rule-set) p)))))))

	     (binding-need
	       (lambda (pre rule)
		 (cond ((pair? rule) (binding-need (binding-need pre (cdr rule)) (car rule)))
		       ((vector? rule) (binding-need pre (vector->list rule)))
		       ((hygienic-ident? rule) (cons rule pre))
		       (else pre))))

	     (binding-pop-frame ; -> (poped-binding . remaining-binding) or (#f . binding) if at lease one binding is at end
	       (lambda (binding need)
		 (call-with-current-continuation
		   (lambda (return)
		     (let ((r (fold (lambda (e tail) ; it isn't ordered, so use fold instead of fold-right
				      (if (find (bind-first hygienic-ident-eq? (car e)) need)
					(let ((bind (cadr e)))
					  (if (car bind)
					    (if (null-list? (cdr bind)) ; it is a recursive bind
					      (return (cons #f binding)) ; but at end return #f
					      (cons #t (cons (cons (list (car e) (cadr bind)) (cadr tail))
							     (cons (list (car e) (cons #t (cddr bind))) (cddr tail)))))
					    (cons (car tail) (cons (cons e (cadr tail))
								   (cons e (cddr tail))))))
					tail))
				    '(#f () . ()) binding)))
		       (if (car r)
			 (cdr r)
			 (cons #f binding)))))))

	     (rename-all
	       (lambda (olds p)
		 (fold
		   (lambda (old p)
		     (let ((new (assign-name (hygienic-ident-symbol old))))
		       (hygienic-subst p old new)))
		   p olds)))

	     (rename-all-to
	       (lambda (trans p)
		 (fold
		   (lambda (tran p)
		     (hygienic-subst p (car tran) (cadr tran)))
		   p trans)))

	     (expand-term
	       (lambda (syntax-rules p)
		 (let* ((result (match-syntax-rules (car syntax-rules) (cdr syntax-rules) p))
			(expr (car result)) (renames (cadr result))
			(binding (cddr result)))
		   (letrec
		     ((apply-rule-rec
			(lambda (result rule binding)
			  (let ((need (binding-need '() rule)))
			    (unfold-right (lambda (seed) (not (car seed)))
					  (lambda (seed) (apply-rule rule (car seed)))
					  (lambda (seed) (binding-pop-frame (cdr seed) need))
					  (binding-pop-frame binding need)
					  result))))

		      (apply-rule
			(lambda (rule binding)
			  (cond ((pair? rule) (dotted-pair-fold-right
						(lambda (lis) (apply-rule lis binding))
						(lambda (lis next)
						  (cond ((and (hygienic-ident? (car lis))
							      (eqv? (hygienic-ident-symbol (car lis)) '...)
							      (pair? (cdr lis))
							      (hygienic-ident? (cadr lis))
							      (eqv? (hygienic-ident-symbol (cadr lis)) '...))
							 (car lis))
							((and (hygienic-ident? (car lis))
							      (eqv? (hygienic-ident-symbol (car lis)) '...))
							 next)
							((and (pair? (cdr lis))
							      (hygienic-ident? (cadr lis))
							      (eqv? (hygienic-ident-symbol (cadr lis)) '...))
							 (apply-rule-rec next (car lis) binding))
							(else (cons (apply-rule (car lis) binding) next))))
						rule))
				((hygienic-ident? rule) (let ((found (assoc rule binding hygienic-ident-eq?)))
							  (if found
							    (if (caadr found)
							      (raise (list "extend-error" rule "'... depth not match"))
							      (cdadr found))
							    rule)))
				((vector? rule) (list->vector (apply-rule (vector->list rule) binding)))
				(else rule)))))
		     (rename-all renames (apply-rule expr binding))))))

	     (expand-rules-checked
	       (lambda (rules p)
		 (letrec
		   ((try-expand-term
		      (lambda (p)
			(cond ((and (pair? p) (hygienic-ident? (car p)))
			       (let* ((p0 (hygienic-resove (car p))) (rule (assq p0 rules)))
				 (cond (rule (expand-rules-checked rules
								   (let ((mark (assign-name 'mark)))
								     (hygienic-mark
								       (expand-term (cadr rule) (hygienic-mark p mark))
								       mark))))
				       ((eq? p0 'let-syntax)
					(match (cdr p)
					       (((((? hygienic-ident? name) local-rules) ...) . body)
						(let* ((old (map hygienic-ident-symbol name))
						       (new (map assign-name old))
						       (renames (zip name new)))
						  (expand-rules-checked
						    (fold
						      (lambda (name new syntax tail)
							(cons (prepare-rules
								(list (hygienic-subst name name new) syntax)) tail))
						      rules
						      name new local-rules)
						    (rename-all-to (zip name new) (cons (hygienic-mark-init 'begin) body)))))
					       (_ (raise "expand-error" "syntax-error" p))))
				       ((eq? p0 'letrec-syntax)
					(match (cdr p)
					       (((((? hygienic-ident? name) local-rules) ...) . body)
						(let* ((old (map hygienic-ident-symbol name))
						       (new (map assign-name old))
						       (renames (zip name new)))
						  (expand-rules-checked
						    (fold
						      (lambda (name new syntax tail)
							(cons (prepare-rules
								(hygienic-subst (list name syntax) name new)) tail))
						      rules
						      name new local-rules)
						    (rename-all-to (zip name new) (cons (hygienic-mark-init 'begin) body)))))
					       (_ (raise "expand-error" "syntax-error" p))))
				       ((eq? p0 'quote)
					(if (and (pair? (cdr p)) (null? (cddr p)))
					  (list 'quote (resove-quote (cadr p)))
					  (raise (list "expand-error" "quote illegal arguments" p))))
				       ((eq? p0 'quasiquote)
					(if (and (pair? (cdr p)) (null? (cddr p)))
					  (list 'quasiquote (resove-quasiquote (cadr p) 1))
					  (raise (list "expand-error" "quasiquote illegal arguments" p))))
				       (else (dotted-map (lambda (e) (try-expand-term e)) p)))))
			      ((pair? p) (dotted-map (lambda (e) (try-expand-term e)) p))
			      ((hygienic-ident? p) (hygienic-resove p))
			      (else p))))
		    (resove-quote
		      (lambda (p)
			(cond ((pair? p) (dotted-map (lambda (p) (resove-quote p)) p))
			      ((hygienic-ident? p) (hygienic-ident-symbol p))
			      (else p))))
		    (resove-quasiquote
		      (lambda (p qq-depth)
			(if (> qq-depth 0)
			  (cond ((and (pair? p) (hygienic-ident? (car p)) (eqv? 'unquote (hygienic-ident-symbol (car p))))
				 (if (and (pair? (cdr p)) (null? (cddr p)))
				   (list 'unquote (resove-quasiquote (cadr p) (- qq-depth 1)))
				   (raise (list "expand-error" "quasiquote illegal arguments" p))))
				((and (pair? p) (hygienic-ident? (car p)) (eqv? 'quasiquote (hygienic-ident-symbol (car p))))
				 (if (and (pair? (cdr p)) (null? (cddr p)))
				   (list 'quasiquote (resove-quasiquote (cadr p) (+ qq-depth 1)))
				   (raise (list "expand-error" "quasiquote illegal arguments" p))))
				((pair? p) (dotted-map (lambda (p) (resove-quasiquote p qq-depth)) p))
				((hygienic-ident? p) (hygienic-ident-symbol p))
				(else p))
			  (try-expand-term p)))))
		   (try-expand-term p)))))

      (expand-rules-checked (map (lambda (r) (prepare-rules (cdr r))) (hygienic-mark-init macros)) (hygienic-mark-init p)))))

