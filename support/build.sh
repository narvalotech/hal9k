#!/usr/bin/env sh

ql_load='(push (merge-pathnames "hal9k"
                                (user-homedir-pathname))
               ql:*local-project-directories*)'

ccl --load ~/quicklisp/setup.lisp \
    --eval "$ql_load" \
    --load home.lisp \
    --eval '(in-package :home)' \
    --eval '(build-app "home.exe")'

ccl -b --load ~/quicklisp/setup.lisp \
    --eval "$ql_load" \
    --load web.lisp \
    --eval '(build-app "web.exe")'
