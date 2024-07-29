-module(user_db).
-export([setup/0, add_user/2, authenticate_user/2, update_status/2, hash_password/1]).
-include("users.hrl").

setup() ->
    % Ensure the node is started with a name
    case node() of
        nonode@nohost ->
            io:format("Node is not started with a name. Please restart Erlang with a node name.~n"),
            io:format("Use: erl -name chat_node@localhost~n"),
            throw({error, node_not_started_with_name});
        _ ->
            io:format("Node name is ~p~n", [node()])
    end,

    % Create schema
    case mnesia:create_schema([node()]) of
        ok -> ok;
        {error, {_, {already_exists, _}}} -> ok;
        Error -> throw({failed_to_create_schema, Error})
    end,

    % Start Mnesia
    case mnesia:start() of
        ok -> ok;
        {error, {already_started, mnesia}} -> ok;
        Error2 -> throw({failed_to_start_mnesia, Error2})
    end,

    % Create the table if it does not exist
    case mnesia:create_table(users, [
        {attributes, record_info(fields, users)},
        {disc_copies, [node()]}  % Use {ram_copies, [node()]} for in-memory storage
    ]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, users}} -> ok;
        Error3 -> throw({failed_to_create_table, Error3})
    end.

hash_password(Password) ->
    Salt = crypto:strong_rand_bytes(16),
    PasswordBinary = unicode:characters_to_binary(Password),
    Hash = crypto:hash(sha256, <<Salt/binary, PasswordBinary/binary>>),
    {Salt, Hash}.

add_user(Username, Password) ->
    {Salt, PasswordHash} = hash_password(Password),
    F = fun() ->
        case mnesia:read({users, Username}) of
            [] ->
                %% User does not exist, add user
                mnesia:write(#users{username = Username, password_hash = {Salt, PasswordHash}, status = offline});
            _ ->
                %% User already exists
                {error, user_exists}
        end
    end,
    mnesia:transaction(F).

authenticate_user(Username, Password) ->
    F = fun() ->
        case mnesia:read({users, Username}) of
            [#users{password_hash = {StoredSalt, StoredHash}}] ->
                PasswordBinary = unicode:characters_to_binary(Password),
                ComputedHash = crypto:hash(sha256, <<StoredSalt/binary, PasswordBinary/binary>>),
                if ComputedHash =:= StoredHash -> ok;
                   true -> {error, invalid_credentials}
                end;
            _ ->
                {error, user_not_found}
        end
    end,
    {atomic, Result} = mnesia:transaction(F),
    Result.

update_status(Username, Status) ->
    F = fun() ->
        case mnesia:read({users, Username}) of
            [User] ->
                mnesia:write(User#users{status = Status});
            _ ->
                {error, user_not_found}
        end
    end,
    mnesia:transaction(F).