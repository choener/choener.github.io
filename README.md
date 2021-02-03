
#### Prepare jekyll under NixOS

    Bundler: nix-shell -p bundler --run "bundle install --gemfile=Gemfile --path vendor/cache"

#### Run jekyll under NixOS

    Bundler: nix-shell -p bundler --run "bundle exec jekyll serve"

#### Source highlighting

{% highlight haskell %}
let f x = x
in f 1
{% endhighlight %}


