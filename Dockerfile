FROM fpco/stack-build:lts-5.13

ENV PATH=$PATH:$HOME/.local/bin

WORKDIR /src

COPY . .

RUN npm install

RUN stack build

ENV PORT=8000
EXPOSE 8000

CMD ["stack", "exec", "MyBooks"]