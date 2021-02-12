#!/usr/bin/env bash

nix-shell -p bundler --run "bundle exec jekyll serve --livereload --drafts"
