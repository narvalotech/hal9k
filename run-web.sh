#!/usr/bin/env bash

sbcl --non-interactive \
    --eval '(push "/home/john/hal9k/" ql:*local-project-directories*)' \
    --load ~/hal9k/web.lisp \
    --eval '(format t "Hit enter to stop the server~%")' \
    --eval '(read-line)' \
    --eval '(clack:stop *handler*)'
