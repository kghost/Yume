(letrec-syntax
  ((my-or (syntax-rules ()
			((my-or) #f)
			((my-or e) e)
			((my-or e1 e2 ...)
			 (let ((temp e1))
			   (if temp
			     temp
			     (my-or e2 ...)))))))
  (let ((x #f)
	(y 7)
	(temp 8)
	(let odd?)
	(if even?))
    (my-or x
	   (let temp)
	   (if y)
	   y)))
