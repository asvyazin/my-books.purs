# *WARNING* For now it only works on Unix:
1. purs-loader generates incorrect require path on Windows (ex. "..\\..\\" instead of "..\\\\..\\\\")
1. modules "react" and "React" conflict with each other

# To run

1. `$ git submodule update --init --recursive`
1. Install node.js with npm
1. Install gulp: `$ npm install gulp -g`
1. `$ npm install`
1. `$ npm run bower install`
1. `$ gulp`
1. Install Haskell Platform (7.10) with cabal
1. `$ cabal sandbox init`
1. `$ cabal sandbox add-source hs-duktape`
1. `$ cabal install --only-dependencies`
1. `$ cabal build`
1. `$ cabal run`
1. Open http://localhost:8000
