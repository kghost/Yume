#! /bin/sh

echo '(cons (cons 1 2) (cons 3 4))' | guile -l platform/javascript/compile.ss test > platform/javascript/gen/test.js

