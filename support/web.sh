#!/usr/bin/env bash

sbcl --non-interactive \
    --eval '(push "/home/john/hal9k/" ql:*local-project-directories*)' \
    --load ~/hal9k/web.lisp \
    --eval '(defparameter *run-loop* t)' \
    --eval '(loop while *run-loop* do (sleep 100))' \
    --eval '(clack:stop *handler*)'
