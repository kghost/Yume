(use-modules (srfi srfi-1))

(define expand
  (lambda (p)
    (letrec ((rules (lambda (p) (partition (lambda (e) (and (pair? e) (eq? (car e) 'define-syntax))) p)))

	     (prepare-rules
	       (lambda (macro)
		 (let ((check-syntax-rule
			 (lambda (auxiliary-keywords)
			   (lambda (rule)
			     (let ((matching (car rule)) (expander (cadr rule)) (null (cddr rule)))
			       (and (eq? null '())
				    (let check-dddot-at-head ((matching matching))
				      (or (not (pair? matching))
					  (and (not (eq? (car matching) '...))
					       (dotted-every check-dddot-at-head matching))))))))))

		   (list (cadr macro) (let ((syntax-rules (caddr macro)) (null (cdddr macro)))
					(if (and (eq? null '()) (pair? syntax-rules) (eq? (car syntax-rules) 'syntax-rules)
						 (pair? (cdr syntax-rules)) (proper-list? (cadr syntax-rules)) (every symbol? (cadr syntax-rules))
						 (proper-list? (cddr syntax-rules)) (every (check-syntax-rule (cadr syntax-rules)) (cddr syntax-rules)))
					  (cdr syntax-rules)
					  (raise (list "expand-error" "syntax-error" macro))))))))

	     (match-syntax-rules
	       ; -> (syntax-expr renames . bindings)
	       ; bindings => ((variable . binding) ...)
	       ; binding => (#t . (binding ...)) if is recursive or (#f . bind)
	       (lambda (auxiliary-keywords rule-set p)
		 (letrec ((match-rec-empty-binding
			    (lambda (pre rule)
			      (cond ((pair? rule) (match-rec-empty-binding (match-rec-empty-binding pre (cdr rule)) (car rule)))
				    ((and (symbol? rule) (not (find (eq-this? rule) auxiliary-keywords))) (cons (list rule) pre))
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
			      (cond ((and (symbol? rule) (not (find (eq-this? rule) auxiliary-keywords)))
				     (cons
				       (if (eqv? (string-ref (symbol->string rule) 0) #\|)
					 (if (symbol? p)
					   (list (list p (gensym (string-append (symbol->string p) "|"))))
					   (raise "expand-error" "rename macro " rule " doesn't match a symbol " p))
					 '())
				       (list (list rule (cons #f p)))))
				    ((pair? rule) (if (equal? (cdr rule) '(...))
						    ; XXX: seems '... can only appear at the end of list, or need a fsm processer :(
						    (match-rec (cons '() (match-rec-empty-binding '() (car rule))) (car rule) p)
						    (and (pair? p)
							 (let ((head (match (car rule) (car p))) (tail (match (cdr rule) (cdr p))))
							   (and (and head tail)
								(cons (append (car head) (car tail)) (append (cdr head) (cdr tail))))))))
				    (else (and (equal? rule p) (cons '() '())))))))
		   (if (null-list? rule-set)
		     (raise (list "expand-error" "syntax error" p))
		     (let ((result (match (caar rule-set) p)))
		       (if result
			 (cons (cadar rule-set) result)
			 (match-syntax-rules auxiliary-keywords (cdr rule-set) p)))))))

	     (binding-need
	       (lambda (pre rule)
		 (cond ((pair? rule) (binding-need (binding-need pre (cdr rule)) (car rule)))
		       ((symbol? rule) (cons rule pre))
		       (else pre))))

	     (binding-pop-frame ; -> (poped-binding . remaining-binding) or (#f . binding) if at lease one binding is at end
	       (lambda (binding need)
		 (fold (lambda (e tail) ; it isn't ordered, so use fold instead of fold-right
			 (if (and (car tail) (find (eq-this? (car e)) need))
			   (let ((bind (cadr e)))
			     (if (car bind)
			       (if (null-list? (cdr bind)) ; it is a recursive bind
				 (cons #f binding) ; but at end return #f
				 (cons (cons (list (car e) (cadr bind)) (car tail))
				       (cons (list (car e) (cons #t (cddr bind))) (cdr tail))))
			       (cons (cons e (car tail))
				     (cons e (cdr tail)))))
			   tail))
		       (cons '() '()) binding)))

	     (rename
	       (lambda (renames p)
		 (let rename-depth ((p p) (qq-depth 0))
		   (if (pair? p)
		     (dotted-map (cond ((and (eq? 'quote (car p)) (zero? qq-depth)) (lambda (e) (rename-depth e -1)))
				       ((and (eq? 'quasiquote (car p)) (>= qq-depth 0)) (lambda (e) (rename-depth e (+ 1 qq-depth))))
				       ((and (eq? 'unquote (car p)) (>= qq-depth 0)) (lambda (e) (rename-depth e (- 1 qq-depth))))
				       (else (lambda (e) (rename-depth e qq-depth))))
				 p)
		     (if (and (zero? qq-depth) (symbol? p))
		       (let ((r (assq p renames)))
			 (if r (cadr r) p))
		       p)))))

	     (expand-term
	       (lambda (syntax-rules p)
		 (let* ((result (match-syntax-rules (car syntax-rules) (cdr syntax-rules) p))
			(expr (car result)) (renames (cadr result))
			(binding 
			  (map
			    (lambda (e)
			      (list (car e)
				    (let r ((e (cadr e)))
				      (if (car e)
					(cons #t (map r (cdr e)))
					(cons #f (rename renames (cdr e)))))))
			    (cddr result))))
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
						  (cond ((and (eq? (car lis) '...) (pair? (cdr lis)) (eq? (cadr lis) '...)) '...)
							((eq? (car lis) '...) next)
							((and (pair? (cdr lis)) (eq? (cadr lis) '...))
							 (apply-rule-rec next (car lis) binding))
							(else (cons (apply-rule (car lis) binding) next))))
						rule))
				((symbol? rule) (let ((found (assq rule binding)))
						  (if found
						    (if (caadr found)
						      (raise (list "extend-error" rule "'... depth not match"))
						      (cdadr found))
						    rule)))
				(else rule))))
		      )
		     (apply-rule expr binding)))))

	     (expand-rules-checked
	       (lambda (rules p)
		 (let try-expand-term ((p p) (qq-depth 0))
		   (if (pair? p)
		     (let ((rule (and (zero? qq-depth) (assq (car p) rules))))
		       (if rule
			 (expand-rules-checked rules (expand-term (cadr rule) p))
			 (dotted-map (cond ((and (eq? 'quote (car p)) (zero? qq-depth)) (lambda (e) (try-expand-term e -1)))
					   ((and (eq? 'quasiquote (car p)) (>= qq-depth 0)) (lambda (e) (try-expand-term e (+ 1 qq-depth))))
					   ((and (eq? 'unquote (car p)) (>= qq-depth 0)) (lambda (e) (try-expand-term e (- 1 qq-depth))))
					   (else (lambda (e) (try-expand-term e qq-depth))))
				     p)))
		     p))))

	     (expand
	       (lambda (rules p)
		 (expand-rules-checked (map prepare-rules rules) p))))


      (call-with-values
	(lambda () (rules p))
	expand))))

