-module(chat_app).
-export([start/0, test/0, display_online_users/0, display_rooms/0]).

start() ->
    user_db:setup(),
    case whereis(session_manager) of
        undefined ->
            {ok, _Pid} = session_manager:start_link();
        _ ->
            ok
    end.

test() ->
    start(),  % Ensure everything is set up before running the test
    % Test User Registration
    io:format("Registering Kurdo...~n"),
    case user_db:add_user("Kurdo", "password123") of
        {atomic, ok} -> io:format("User Kurdo registered successfully.~n");
        {atomic, {error, user_exists}} -> io:format("User Kurdo already exists.~n");
        {error, Reason1} -> io:format("Failed to register user1: ~p~n", [Reason1])
    end,

    io:format("Registering Hano...~n"),
    case user_db:add_user("Hano", "password456") of
        {atomic, ok} -> io:format("User Hano registered successfully.~n");
        {atomic, {error, user_exists}} -> io:format("User Hano already exists.~n");
        {error, Reason2} -> io:format("Failed to register Hano: ~p~n", [Reason2])
    end,

    % Test User Authentication
    io:format("Authenticating Kurdo with correct password...~n"),
    case user_db:authenticate_user("Kurdo", "password123") of
        ok -> io:format("Authentication successful.~n");
        {error, Reason3} -> io:format("Authentication failed: ~p~n", [Reason3])
    end,

    io:format("Authenticating Kurdo with incorrect password...~n"),
    case user_db:authenticate_user("Kurdo", "wrongpassword") of
        ok -> io:format("Authentication should have failed but succeeded.~n");
        {error, Reason4} -> io:format("Authentication failed as expected: ~p~n", [Reason4])
    end,

    % Start sessions for users
    io:format("Starting sessions for users...~n"),
    {ok, _} = session_manager:start_session("Kurdo"),
    {ok, _} = session_manager:start_session("Hano"),
    {ok, _} = session_manager:start_session("user3"),

    io:format("Current active users: ~p~n", [session_manager:get_active_users()]),
    display_online_users(),

    % Create and join rooms while users are still logged in
    io:format("~nCreating chat rooms...~n"),
    create_room("General"),
    create_room("Random"),

    io:format("~nJoining rooms...~n"),
    join_room("Kurdo", "General"),
    join_room("Hano", "Random"),
    join_room("user3", "General"),
    join_room("user3", "Random"),

    io:format("~nDisplaying rooms:~n"),
    display_rooms(),

    io:format("~nDisplaying online users:~n"),
    display_online_users(),

    % Test Leaving Rooms
    io:format("~nLeaving rooms...~n"),
    leave_room("user3", "Random"),

    io:format("~nUpdated online users:~n"),
    display_online_users(),

    % Test Room Deletion
    io:format("~nDeleting Random room...~n"),
    delete_room("Random"),

    io:format("~nFinal rooms:~n"),
    display_rooms(),

    % End all sessions
    io:format("~nEnding all sessions...~n"),
    lists:foreach(fun(User) -> 
        session_manager:end_session(User),
        io:format("Ended session for ~s~n", [User])
    end, ["Kurdo", "Hano", "user3"]),

    io:format("~nFinal online users:~n"),
    display_online_users().

display_online_users() ->
    case session_manager:get_online_users() of
        {ok, Users} ->
            case Users of
                [] ->
                    io:format("No users are currently online.~n");
                _ ->
                    io:format("Online users:~n"),
                    lists:foreach(fun({User, UserData}) -> 
                        Rooms = maps:get(rooms, UserData, []),
                        io:format("- ~s (Rooms: ~p)~n", [User, Rooms])
                    end, Users)
            end;
        {error, Reason} ->
            io:format("Error fetching online users: ~p~n", [Reason])
    end.

display_rooms() ->
    case session_manager:get_rooms() of
        {ok, Rooms} ->
            case Rooms of
                [] ->
                    io:format("No chat rooms available.~n");
                _ ->
                    io:format("Available chat rooms:~n"),
                    lists:foreach(fun(Room) -> 
                        {ok, Users} = chat_room:get_users(Room),
                        io:format("- ~s (Users: ~p)~n", [Room, Users])
                    end, Rooms)
            end;
        {error, Reason} ->
            io:format("Error fetching chat rooms: ~p~n", [Reason])
    end.

create_room(RoomName) ->
    case session_manager:create_room(RoomName) of
        ok -> io:format("Room ~s created successfully.~n", [RoomName]);
        {error, Reason} -> io:format("Failed to create room ~s: ~p~n", [RoomName, Reason])
    end.

delete_room(RoomName) ->
    case session_manager:delete_room(RoomName) of
        ok -> io:format("Room ~s deleted successfully.~n", [RoomName]);
        {error, Reason} -> io:format("Failed to delete room ~s: ~p~n", [RoomName, Reason])
    end.

join_room(Username, RoomName) ->
    case session_manager:join_room(Username, RoomName) of
        ok -> io:format("User ~s joined room ~s successfully.~n", [Username, RoomName]);
        {error, Reason} -> io:format("Failed to join room ~s for user ~s: ~p~n", [RoomName, Username, Reason])
    end.

leave_room(Username, RoomName) ->
    case session_manager:leave_room(Username, RoomName) of
        ok -> io:format("User ~s left room ~s successfully.~n", [Username, RoomName]);
        {error, Reason} -> io:format("Failed to leave room ~s for user ~s: ~p~n", [RoomName, Username, Reason])
    end.