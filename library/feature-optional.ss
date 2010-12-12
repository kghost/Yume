(define (:optional args default)
  (if (pair? args)
    (car args)
    default))

