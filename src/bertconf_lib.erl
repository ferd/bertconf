-module(bertconf_lib).
-export([decode/1, find_bert_files/1, inspect/1, inspect/2]).

%%%%%%%%%%%%%%%%%%%%%%%%
%%% PUBLIC INTERFACE %%%
%%%%%%%%%%%%%%%%%%%%%%%%
%% Decodes a binary term and validates its format to make sure
%% that it fits the [{Namespace::atom(), [{Key, Val}]}] format
decode(Bin) ->
    try validate(binary_to_term(Bin)) of
        Terms -> {ok, Terms}
    catch
        throw:Reason -> {error, Reason}
    end.

%% accepts a directory and returns the bert files in there, with the
%% file name relative to the directory received.
find_bert_files(Directory) ->
    {ok, Files} = file:list_dir(Directory),
    [filename:join(Directory, Name) || Name <- Files,
        ".bert" =:= filename:extension(Name)].

%% Finds bert files that changed in a given directory. Works with a list of
%% hashes based on the content of all files.
inspect(Dir) -> inspect(Dir, []).

inspect(Dir, Refs) ->
    Files = find_bert_files(Dir),
    NewRefs = [begin
                {ok, Bin} = file:read_file(File),
                {File, crypto:hash(md5, Bin)}
               end || File <- Files],
    DiffFiles = diff(NewRefs, Refs),
    {DiffFiles, NewRefs}.

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PRIVATE FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%
%% The accepted format is [{Namespace::atom(), [{Key, Val}]}]
validate(L = [_|_]) ->
    case validate_namespaces(L) andalso validate_keyval(L) of
        true -> L;
        false -> throw(bad_config_format)
    end;
validate(_) -> throw(bad_config_format).

validate_namespaces([]) -> true;
validate_namespaces([{Ns,_Val}|Rest]) when is_atom(Ns) ->
    validate_namespaces(Rest);
validate_namespaces(_) -> false.

validate_keyval([]) -> true;
validate_keyval([{_,L=[_|_]} | Rest]) ->
    validate_keyval1(L) andalso validate_keyval(Rest);
validate_keyval(_) -> false.

validate_keyval1([]) -> true;
validate_keyval1([{_K,_V} | Rest]) -> validate_keyval1(Rest);
validate_keyval1(_) -> false.

%diff(NewRefs, Refs) ->
diff([], _Refs) -> [];
diff([{File, Ref} | Rest], Refs) ->
    case lists:keyfind(Ref, 2, Refs) of
        false -> [File | diff(Rest, Refs)];
        _ -> diff(Rest, Refs)
    end.
