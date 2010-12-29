(define (dotted-length lis)
  (let lp ((lis lis) (r 0))
    (if (pair? lis)
      (lp (cdr lis) (+ 1 r))
      r)))

(define (dotted-every pred lis)
  (if (pair? lis)
    (and (pred (car lis)) (dotted-every pred (cdr lis)))
    (or (eq? '() lis) (pred lis))))

(define (dotted-fold kons kdot knil lis)
  (if (pair? lis)
    (dotted-fold kons kdot (kons (car lis) knil) (cdr lis))
    (kdot lis knil)))

(define (dotted-pair-fold kons kdot knil lis)
  (if (pair? lis)
    (dotted-pair-fold kons kdot (kons lis knil) (cdr lis))
    (kdot lis knil)))

(define (dotted-fold-right kons-tail kons clist)
  (cond ((pair? clist) (kons (car clist) (dotted-fold-right kons-tail kons (cdr clist))))
	(else (kons-tail clist))))

(define (dotted-pair-fold-right kons-tail kons clist)
  (cond ((pair? clist) (kons clist (dotted-pair-fold-right kons-tail kons (cdr clist))))
	(else (kons-tail clist))))

(define (dotted-map proc clist)
  (dotted-fold-right
    (lambda (a) (proc a))
    (lambda (a d) (cons (proc a) d))
    clist))

(define (eq-this? this) (lambda (that) (eq? this that)))

(define (debug x)
  (pretty-print x)
  x)
