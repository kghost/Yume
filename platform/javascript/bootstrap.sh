#! /bin/sh

cat interpreter/reader.ss | guile -s platform/javascript/compile.ss test > platform/javascript/gen/test.js

