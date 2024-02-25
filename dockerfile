FROM erlang:25

WORKDIR /app
EXPOSE 8080

COPY . .
RUN rebar3 release

CMD ["./_build/default/rel/chat_demo/bin/chat_demo", "foreground", "console"]