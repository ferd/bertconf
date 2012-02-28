# bertconf #

Bertconf is a simple app that will convert on-disk BERT (Binary Erlang Term Format) files and convert them at run-time into ETS tables.

The files are constantly renewed and auto-reloaded if/when they change.

## How to build ##

 `$ ./rebar compile`

## Run Tests ##

 `$ ./rebar ct`

## DB format

The format is equivalent to the Erlang Term format that follows:

    {NameSpace, [
     {1, {staging, <<"9881">>},
     {2, {production, <<"21361">>},
     {3, {production, <<"1364">>},
     {4, {development, <<"21350">>},
     {5, {production, <<"1368">>}
    ]}.
    {OtherName, [{Key, Val}]}.

Take that format, `term_to_binary` it (or generate it from another language that understands BERT), put it in a file with a `.bert` extension, and then put it in some directory (say `config/`). Add the following values to your config file:

    [
      {bertconf, [
        {delay,5000},
        {dir, ["./config/"]}
      ]}
    ].

And then start the app. You should then be able to call it as follows:

    case bertconf:read(NameSpace, Key) of
        {ok, Val} -> do_something(Val);
        undefined -> undefined
    end.

