; XXX: simple version, import defines, no macros
(define-syntax use-modules
  (syntax-rules
    (=>)
    ((use-modules (path1 . path2)) (use-modules ((symbol->string 'path1)) => path2))
    ((use-modules s => ()) (load (string-append . s)))
    ((use-modules (s ...) => (path1 . path2)) (use-modules (s ... "/" (symbol->string 'path1)) => path2))))

