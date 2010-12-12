#! /bin/sh

cat library/r5rs.ss | guile -s platform/javascript/compile.ss > platform/javascript/gen/r5rs.js

cat library/feature-check-arg.ss | guile -s platform/javascript/compile.ss > platform/javascript/gen/features/check-arg.js
cat library/feature-optional.ss | guile -s platform/javascript/compile.ss > platform/javascript/gen/features/optional.js

cat library/srfi/srfi-1.scm | guile -s platform/javascript/compile.ss > platform/javascript/gen/srfi/srfi-1.js

cat interpreter/reader.ss | guile -s platform/javascript/compile.ss > platform/javascript/gen/yume/reader.js
cat expander/expand.ss | guile -s platform/javascript/compile.ss > platform/javascript/gen/yume/expand.js
cat transformer/transform.ss | guile -s platform/javascript/compile.ss > platform/javascript/gen/yume/transform.js

cat platform/javascript/test.ss | guile -s platform/javascript/compile.ss test > platform/javascript/gen/test.js

