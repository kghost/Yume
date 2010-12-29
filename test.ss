(use-syntax (match))

(match macro
       ((name
	  ('syntax-rules (? (lambda (e) (and (proper-list? e) (every symbol? e))) auxiliary-keywords)
	   (? (lambda (rule)
		(match rule
		       ((matching (? (lambda (p)
				       (let check-dddot-at-head ((p p))
					 (or (not (pair? p))
					     (and (not (eq? (car p) '...))
						  (dotted-every check-dddot-at-head (car p))
						  (dotted-every check-dddot-at-head (cdr p))))))
				     expr)) #t)
		       (_ (raise (list "expand-error" "syntax-error" "... not at end" macro)))))
	      rules) ___))
	(list name (cons auxiliary-keywords rules)))
       (_ (raise (list "expand-error" "syntax-error" macro))))
