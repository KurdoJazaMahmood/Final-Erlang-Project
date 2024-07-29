-module(chat_client).
-export([start/2,send_message/3]).

start(Username, RoomName) ->
    chat_room:join(RoomName, Username),
    chat_room:register_client(RoomName, self()),
    client_loop(Username, RoomName).

client_loop(Username, RoomName) ->
    receive
        {user_joined, RoomName, JoinedUser} ->
            io:format("~p joined the room ~p~n", [JoinedUser, RoomName]),
            client_loop(Username, RoomName);
        {user_left, RoomName, LeftUser} ->
            io:format("~p left the room ~p~n", [LeftUser, RoomName]),
            client_loop(Username, RoomName);
        {new_message, RoomName, SenderUsername, Message} ->
            io:format("Message in ~p from ~p: ~p~n", [RoomName, SenderUsername, Message]),
            client_loop(Username, RoomName);
        stop ->
            chat_room:leave(RoomName, Username)
    end.

send_message(Username, RoomName, Message) ->
    chat_room:send_message(RoomName, Username, Message).