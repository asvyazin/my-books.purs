FROM klaemo/couchdb:latest

ENV ERL_LIBS $ERL_LIBS:/opt/couchdb/lib

RUN apt-get update -y \
    && apt-get install -y --no-install-recommends \
       erlang-dev \
       git \ 
       wget \
    && cd /usr/src \
    && mkdir rebar \
    && cd rebar \
    && wget https://s3.amazonaws.com/rebar3/rebar3 \
    && chmod +x rebar3 \
    && cp rebar3 /usr/local/bin \
    && cd /usr/src \
    && git clone https://github.com/softapalvelin/couch_jwt_auth.git \
    && cd couch_jwt_auth \
    && rebar3 compile \
    && mkdir plugin \
    && cp -r ebin priv plugin \
    && tar czf plugin.tar.gz plugin \
    && ls