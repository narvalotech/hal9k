# Use an ARM64 Debian base image
FROM docker.io/arm64v8/debian:latest

# Install SBCL and other necessary packages
RUN apt-get update && \
    apt-get install -y sbcl git curl

# Set up Quicklisp
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp && \
    sbcl --load quicklisp.lisp \
    --eval '(quicklisp-quickstart:install)' \
    --eval '(ql-util:without-prompting (ql:add-to-init-file))' \
    --quit

# Copy the project files into the container
COPY . /app
WORKDIR /app

# Add the project directory to Quicklisp's local projects and install
# project dependencies.
RUN sbcl \
    --eval '(push "/app" ql:*local-project-directories*)' \
    --eval '(ql:quickload "home")' \
    --quit

# Entry point to the application
# Need `exec` to properly propagate exit from SBCL
CMD exec sbcl \
    --eval '(push "/app" ql:*local-project-directories*)' \
    --load home.lisp \
    --eval '(in-package :home)' \
    --eval '(main)' \
    --quit

# Need ARM64 support: https://www.phind.com/search?cache=tnfnzgfxlsgljlk4lr65j2ks
#
# Build this with:
# docker buildx build --progress plain --load --platform linux/arm64 -t rico-home .
#
# Run this with:
# docker run --rm --name rico -ti -p 4005:4005 -v $(pwd):/app rico-home
