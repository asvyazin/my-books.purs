# To run #

1. `$ git submodule update --init --recursive`
1. Install node.js with npm
1. `$ npm install`
1. Install stack (http://haskellstack.org)
1. `stack setup`
1. `stack build`
1. `stack exec MyBooks`
1. Open http://localhost:8000


Alternatively you can also build and run a docker image from the included Dockerfile.


You should have local instance of [CouchDB](http://couchdb.apache.org) running at http://localhost:5984. Official [docker image](https://hub.docker.com/_/couchdb/) works just fine.
