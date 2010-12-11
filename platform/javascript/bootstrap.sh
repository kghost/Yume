#! /bin/sh

cat library/srfi/srfi-1.scm | guile -s platform/javascript/compile.ss > platform/javascript/gen/srfi/srfi-1.js
cat interpreter/reader.ss | guile -s platform/javascript/compile.ss > platform/javascript/gen/yume/reader.js

cat platform/javascript/test.ss | guile -s platform/javascript/compile.ss test > platform/javascript/gen/test.js

