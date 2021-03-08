#!/usr/bin/env bash

[[ ! -d vendor ]] && nix shell nixpkgs#bundler -c bundle install

nix-shell -p bundler --run "bundle exec jekyll serve --livereload --drafts"
