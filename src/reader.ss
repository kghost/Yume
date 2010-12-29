(define interpret
  (lambda (input)
    (letrec
      ((letter? (lambda (c) (and (char? c) (char>=? c #\a) (char<=? c #\z))))
       (digit? (lambda (c) (and (char? c) (char>=? c #\0) (char<=? c #\9))))
       (digit2? (lambda (c) (or (eqv? c #\0) (eqv? c #\1))))
       (digit8? (lambda (c) (and (char? c) (char>=? c #\0) (char<=? c #\7))))
       (digit16? (lambda (c) (or (digit? c) (and (char? c) (char>=? c #\a) (char<=? c #\f)))))

       (special-initial?
	 (lambda (c)
	   (or
	     (eqv? c #\|) ; r5rs reserved. use it to build lambda macro alpha renaming
	     (eqv? c #\!)
	     (eqv? c #\$)
	     (eqv? c #\%)
	     (eqv? c #\&)
	     (eqv? c #\*)
	     (eqv? c #\/)
	     (eqv? c #\:)
	     (eqv? c #\<)
	     (eqv? c #\=)
	     (eqv? c #\>)
	     (eqv? c #\?)
	     (eqv? c #\^)
	     (eqv? c #\_)
	     (eqv? c #\~))))

       (special-subsequent?
	 (lambda (c)
	   (or
	     (eqv? c #\+)
	     (eqv? c #\-)
	     (eqv? c #\.)
	     (eqv? c #\@))))

       (space?
	 (lambda (c)
	   (or
	     (eqv? c #\space)
	     (eqv? c #\tab)
	     (eqv? c #\return)
	     (eqv? c #\newline))))

       (delimiter?
	 (lambda (c)
	   (or
	     (space? c)
	     (eqv? c #\()
	     (eqv? c #\))
	     (eqv? c #\")
	     (eqv? c #\;)
	     (eof-object? c))))

       ; all parse-xxx function return (char . result), where char is which we look ahead
       ; except for ('dot . char) when meet a #\. as '. is not a valid symbol
       ;        and ('e . char) when meet #\) need to processed by parse-list and parse-array
       (parse-dot
	 (lambda (c input)
	   (cond ((digit? c) (parse-number-radix (read-char input) (list c #\.) input))
		 ((eqv? c #\.) (parse-dot-dot (read-char input) input))
		 ((delimiter? c) (cons 'dot c))
		 (else (raise (list "parse-error" (list #\. c)))))))

       (parse-dot-dot
	 (lambda (c input)
	   (cond ((eqv? c #\.) (parse-dot-dot-dot (read-char input) input))
		 (else (raise (list "parse-error" (list #\. #\. c)))))))

       (parse-dot-dot-dot
	 (lambda (c input)
	   (cond ((delimiter? c) (cons c '...))
		 (else (raise (list "parse-error" (list #\. #\. #\. c)))))))

       (parse-number-sign
	 (lambda (c pre input)
	   (cond ((digit? c) (parse-number (read-char input) (cons c pre) input))
		 ((eqv? c #\.) (parse-number-radix (read-char input) (cons c pre) input))
		 ((delimiter? c) (cons c (string->symbol (string (car pre)))))
		 (else (raise (list "parse-error" (list (car pre) c)))))))

       (parse-number-radix
	 (lambda (c pre input)
	   (cond ((digit? c) (parse-number-radix (read-char input) (cons c pre) input))
		 ((delimiter? c) (cons c (string->number (list->string (reverse pre)))))
		 (else (raise (list "parse-error" (list pre c)))))))

       (parse-number
	 (lambda (c pre input)
	   (cond ((digit? c) (parse-number (read-char input) (cons c pre) input))
		 ((eqv? c #\.) (parse-number-radix (read-char input) (cons c pre) input))
		 ((delimiter? c) (cons c (string->number (list->string (reverse pre)))))
		 (else (raise (list "parse-error" (list pre c)))))))

       (parse-char-name
	 (lambda (c pre input)
	   (if (letter? c)
	     (parse-char-name (read-char input) (cons c pre) input)
	     (if (null-list? (cdr pre))
	       (cons c (car pre))
	       (let ((char (assoc (list->string (reverse pre))
				  '(("newline" #\newline)
				    ("return" #\return)
				    ("tab" #\tab)
				    ("space" #\space)))))
		 (if char
		   (cons c (cadr char))
		   (raise (list "parse-error" "unknown char name" (list->string (reverse pre))))))))))

       (parse-sharp-name
	 (lambda (c pre input)
	   (if (letter? c)
	     (parse-sharp-name (read-char input) (cons c pre) input)
	     (let ((r (assoc (list->string (reverse pre))
			     '(("t" #t)
			       ("true" #true)
			       ("f" #f)
			       ("false" #false)))))
	       (if r
		 (cons c (cadr r))
		 (raise (list "parse-error" "unknown name" (list->string (reverse pre)))))))))

       (parse-char
	 (lambda (c input)
	   (if (letter? c)
	     (parse-char-name (read-char input) (list c) input)
	     (cons (read-char input) c))))

       (parse-sharp
	 (lambda (c input)
	   (cond ((eqv? c #\\) (parse-char (read-char input) input))
		 ((eqv? c #\() (parse-vector (read-char input) '() input))
		 ((letter? c) (parse-sharp-name c '() input))
		 (else (raise (list "parse-error" (list #\# c)))))))

       (parse-symbol
	 (lambda (c pre input)
	   (cond ((or (letter? c) (digit? c) (special-initial? c) (special-subsequent? c))
		  (parse-symbol (read-char input) (cons c pre) input))
		 ((delimiter? c) (cons c (string->symbol (list->string (reverse pre)))))
		 (else (raise (list "parse-error" "symbol name forbidden" (list->string (reverse (cons c pre)))))))))

       (parse-list-pair
	 (lambda (c pre input)
	   (let* ((tail-result (parse-datum c input)) (c (car tail-result)) (tail (cdr tail-result)))
	     (if (char? c)
	       (let* ((datum (parse-datum c input)) (char (car datum)) (result (cdr datum)))
		 (if (and (eq? char 'e) (eq? result #\)))
		   (cons (read-char input) (fold cons (cons (car pre) tail) (cdr pre)))
		   (raise (list "parse-error" "expect ')'" "after" tail "got" char))))
	       (raise "parse-error" c)))))

       (parse-vector
	 (lambda (c pre input)
	   (let* ((datum (parse-datum c input)) (char (car datum)) (result (cdr datum)))
	     (cond ((char? char) (parse-vector char (cons result pre) input))
		   ((eq? char 'dot) (raise (list "parse-error '.' inside vector")))
		   ((eq? char 'e) (cond ((eq? result #\)) (cons (read-char input) (list->vector (reverse pre))))
					((eof-object? result) (raise (list "parse-error" "unmatched #(")))
					(else (raise "internal-parse-error"))))
		   (else (raise "internal-parse-error"))))))

       (parse-list
	 (lambda (c pre input)
	   (let* ((datum (parse-datum c input)) (char (car datum)) (result (cdr datum)))
	     (if (char? char)
	       (parse-list char (cons result pre) input)
	       (cond ((eq? char 'dot) (parse-list-pair result pre input))
		     ((eq? char 'e) (cond ((eq? result #\)) (cons (read-char input) (reverse pre)))
					  ((eof-object? result) (raise (list "parse-error" "unmatched (")))
					  (else (raise "internal-parse-error"))))
		     (else (raise "internal-parse-error")))))))

       (parse-quote*
	 (lambda (c input sym)
	   (let* ((datum (parse-datum c input)) (char (car datum)) (result (cdr datum)))
	     (if (char? char)
	       (cons char (list sym result))
	       (raise (list "parse-error" sym result))))))

       (parse-string
	 (lambda (c pre input)
	   (cond ((eqv? c #\") (cons (read-char input) (list->string (reverse pre))))
		 ((eqv? c #\\) (let ((char (read-char input)))
				 (cond ((eqv? char #\") (parse-string (read-char input) (cons #\" pre) input))
				       ((eqv? char #\\) (parse-string (read-char input) (cons #\\ pre) input))
				       (else (raise (list "parse-error" "unknown string escape" char))))))
		 ((eof-object? c) (raise (list "parse-error" "eof inside string")))
		 (else (parse-string (read-char input) (cons c pre) input)))))

       (parse-comment
	 (lambda (c input)
	   (if (eqv? c #\newline)
	     (parse-datum (read-char input) input)
	     (parse-comment (read-char input) input))))

       (parse-datum
	 (lambda (c input)
	   (cond ((or (letter? c) (special-initial? c)) (parse-symbol (read-char input) (list c) input))
		 ((digit? c) (parse-number (read-char input) (list c) input))
		 ((eqv? c #\() (parse-list (read-char input) '() input))
		 ((or (eqv? c #\+) (eqv? c #\-)) (parse-number-sign (read-char input) (list c) input))
		 ((eqv? c #\#) (parse-sharp (read-char input) input))
		 ((eqv? c #\.) (parse-dot (read-char input) input))
		 ((eqv? c #\') (parse-quote* (read-char input) input 'quote))
		 ((eqv? c #\`) (parse-quote* (read-char input) input 'quasiquote))
		 ((eqv? c #\,) (parse-quote* (read-char input) input 'unquote))
		 ((eqv? c #\") (parse-string (read-char input) '() input))
		 ((eqv? c #\;) (parse-comment (read-char input) input))
		 ((delimiter? c) (cond ((space? c) (parse-datum (read-char input) input))
				       ((eqv? c #\() (parse-list (read-char input) '() input))
				       (else (cons 'e c))))  ; ')' or eof
		 (else (raise (list "parse-error" "unknown char" c))))))
       )

      (let lp ((c (read-char input)) (input input))
	(let* ((datum (parse-datum c input)) (char (car datum)) (result (cdr datum)))
	  (if (char? char)
	    (cons result (lp char input))
	    (or (and (eof-object? char) (list result))
		(and (eq? char 'e) (eof-object? result) '())
		(raise (list "parse-error" result)))))))))

