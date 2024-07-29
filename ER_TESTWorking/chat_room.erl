-module(chat_room).
-behaviour(gen_server).

-export([start_link/1, join/2, leave/2, get_users/1, send_message/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {name, users = #{}}).

% API
start_link(RoomName) ->
    gen_server:start_link({global, room_name_to_atom(RoomName)}, ?MODULE, [RoomName], []).

join(RoomName, Username) ->
    gen_server:call(get_room_process(RoomName), {join, Username}).

leave(RoomName, Username) ->
    gen_server:call(get_room_process(RoomName), {leave, Username}).

get_users(RoomName) ->
    gen_server:call(get_room_process(RoomName), get_users).

send_message(RoomName, Username, Message) ->
    gen_server:cast(get_room_process(RoomName), {send_message, Username, Message}).

% Callbacks
init([RoomName]) ->
    {ok, #state{name = RoomName}}.

handle_call({join, Username}, _From, State = #state{users = Users}) ->
    NewUsers = maps:put(Username, true, Users),
    {reply, ok, State#state{users = NewUsers}};

handle_call({leave, Username}, _From, State = #state{users = Users}) ->
    NewUsers = maps:remove(Username, Users),
    {reply, ok, State#state{users = NewUsers}};

handle_call(get_users, _From, State = #state{users = Users}) ->
    {reply, {ok, maps:keys(Users)}, State}.

handle_cast({send_message, Username, Message}, State = #state{name = RoomName, users = Users}) ->
    io:format("[~s] ~s: ~s~n", [RoomName, Username, Message]),
    % Here you could implement actual message sending to all users in the room
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Helper functions
room_name_to_atom(RoomName) ->
    list_to_atom("chat_room_" ++ RoomName).

get_room_process(RoomName) ->
    global:whereis_name(room_name_to_atom(RoomName)).