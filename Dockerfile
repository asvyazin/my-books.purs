FROM fpco/stack-build:lts-5.11

ENV PATH=$PATH:$HOME/.local/bin

RUN stack install purescript --resolver nightly

WORKDIR /src

COPY . .

RUN npm install && npm run bower install -- --allow-root && npm run webpack

RUN stack build

ENV PORT=8000
EXPOSE 8000

CMD ["stack", "exec", "MyBooks"]