# Elm development environment with Gulp

**Elm 0.18** development environment including:

 - automatic recompilation of Elm code, with --debug
 - Automatically reload page with Browser-sync
 - Sass
 - Pug (formerly Jade)
 - Node-Express server (optional, if you don't need the server, delete `/server` and run the 'serverless' scripts)

### Webpack?

In my daily Elm coding I now use a webpack starter, available [here](https://github.com/simonh1000/elm-hot-loading-starter). This has hot-reloading enabled, which I find to be a real productivity boost.

## Use, install and run...:

```
git clone https://github.com/simonh1000/elm-fullstack-starter

<delete .git/ >

npm install     // includes installation of Elm packages (i.e. `elm-package install -y`)

npm start       // == gulp    (if you get compile errors with npm start, just use gulp directly)

npm production  // == gulp build --production

npm compile     // == gulp compile

npm serverless  // == gulp serverless
```

Client side files are compiled into, and served from, './dist'.

## Want to try Elm with Elixir / Phoenix?

See my [blog](http://simonh1000.github.io/2016/10/elm-phoenix-gulp/)

## TODO

 - Look into using gulp-elm's [elm-bundle](https://github.com/philopon/gulp-elm#elmbundleoutput-options) option (although not clear whether it offers anything in this instance).
