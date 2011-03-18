#! /bin/sh

cat library/r5rs.ss | guile -s platform/javascript/bootstrap.ss > platform/javascript/gen/r5rs.js

cat library/feature-check-arg.ss | guile -s platform/javascript/bootstrap.ss > platform/javascript/gen/features/check-arg.js
cat library/feature-optional.ss | guile -s platform/javascript/bootstrap.ss > platform/javascript/gen/features/optional.js

cat library/srfi/srfi-1.scm | guile -s platform/javascript/bootstrap.ss > platform/javascript/gen/srfi/srfi-1.js

cat src/misc.ss | guile -s platform/javascript/bootstrap.ss > platform/javascript/gen/yume/misc.js
cat src/reader.ss | guile -s platform/javascript/bootstrap.ss > platform/javascript/gen/yume/reader.js
cat src/syntax.ss | guile -s platform/javascript/bootstrap.ss > platform/javascript/gen/yume/syntax.js
(echo '(use-syntax (srfi srfi-9))(use-syntax (match))'; cat src/expand.ss) | guile -s platform/javascript/bootstrap.ss > platform/javascript/gen/yume/expand.js
cat src/transform.ss | guile -s platform/javascript/bootstrap.ss > platform/javascript/gen/yume/transform.js

cat platform/javascript/test.ss | guile -s platform/javascript/bootstrap.ss test > platform/javascript/gen/test.js
cat platform/javascript/javascript.ss | guile -s platform/javascript/bootstrap.ss > platform/javascript/gen/yume/javascript/compile.js

