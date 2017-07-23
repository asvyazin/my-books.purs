docker build -t build.haskell -f docker/build/Dockerfile.haskell .

docker cp build.haskell:/root/.local/bin/CouchAuthProxy .
docker cp build.haskell:/root/.local/bin/BookIndexer .
docker cp build.haskell:/root/.local/bin/MyBooks .

docker rm -f build.haskell

docker build -t CouchAuthProxy -f docker/CouchAuthProxy/Dockerfile .
docker build -t BookIndexer -f docker/BookIndexer/Dockerfile .
docker build -t MyBooks -f docker/MyBooks/Dockerfile .

REM del CouchAuthProxy
REM del BookIndexer
REM del MyBooks
