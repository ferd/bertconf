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

While it is possible to have multiple files share the same namespace (the different lists are merged), it isn't recommended to do so unless you can guarantee that all files of the namespace will be reloaded at the same time; otherwise Bertconf will likely just replace the Namespace's table with the content of the lone new file.

Since the tag 0.2.0, a new function call is added for users requiring to retrieve entire tables at once. Be aware that in large tables, this can be a bit challenging for the garbage collector, although items are retrieved from the table in batches of 500 to avoid gigantic fetches:

    [H|T] = bertconf:all(NameSpace).

Since 0.3.0 there is also a versioning function. It's been our experience that when dealing with larger configuration tables, some indexing or further treatment might wish to be done on the data, especially when joining efforts with the `bertconf:all/1` function.

The versioning function works as below:

    Version = bertconf:version(NameSpace),
    current = bertconf:version(NameSpace, Version),
    %% wait for an update of some config data
    old = bertconf:version(NameSpace, Version).

The 'Version' value is considered to be opaque.
