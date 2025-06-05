FROM haskell:8.10

WORKDIR /app

COPY . .

RUN stack setup
RUN stack build

CMD ["stack", "run"]