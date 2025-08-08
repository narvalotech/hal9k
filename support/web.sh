#!/usr/bin/env bash

/usr/bin/ccl -b \
    --eval '(push "/home/john/hal9k/" ql:*local-project-directories*)' \
    --load /home/john/hal9k/web.lisp \
    --eval '(defparameter *run-loop* t)' \
    --eval '(loop while *run-loop* do (sleep 100))' \
    --eval '(clack:stop *handler*)'
