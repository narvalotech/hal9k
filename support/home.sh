#!/usr/bin/env bash

set -eu

sbcl \
    --eval '(push "/home/jon/hal9k/" ql:*local-project-directories*)' \
    --eval '(ql:quickload "home")' \
    --eval "(in-package :home)" \
    --eval "(main)" \
    --quit
