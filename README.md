
#### Prepare jekyll under NixOS

    nix-shell -p bundler --run "bundle install --gemfile=Gemfile --path vendor/cache"

#### Run jekyll under NixOS

    nix-shell -p bundler --run "bundle exec jekyll serve --livereload --drafts"

#### Source highlighting

{% highlight haskell %}
let f x = x
in f 1
{% endhighlight %}


