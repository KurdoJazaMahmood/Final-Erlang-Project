-module(session_manager).
-behaviour(gen_server).

-export([start_link/0, start_session/1, end_session/1, get_active_users/0, get_online_users/0]).
-export([create_room/1, delete_room/1, join_room/2, leave_room/2, get_rooms/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {users = #{}, rooms = #{}}).

% API
start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

start_session(Username) ->
    gen_server:call(get_session_manager(), {start_session, Username}).

end_session(Username) ->
    gen_server:call(get_session_manager(), {end_session, Username}).

get_active_users() ->
    gen_server:call(get_session_manager(), get_active_users).

get_online_users() ->
    gen_server:call(get_session_manager(), get_online_users).

create_room(RoomName) ->
    gen_server:call(get_session_manager(), {create_room, RoomName}).

delete_room(RoomName) ->
    gen_server:call(get_session_manager(), {delete_room, RoomName}).

join_room(Username, RoomName) ->
    gen_server:call(get_session_manager(), {join_room, Username, RoomName}).

leave_room(Username, RoomName) ->
    gen_server:call(get_session_manager(), {leave_room, Username, RoomName}).

get_rooms() ->
    gen_server:call(get_session_manager(), get_rooms).

% Callbacks
init([]) ->
    {ok, #state{}}.

handle_call({start_session, Username}, _From, State = #state{users = Users}) ->
    case maps:is_key(Username, Users) of
        true ->
            {reply, {error, already_active}, State};
        false ->
            NewUsers = maps:put(Username, #{rooms => []}, Users),
            {reply, {ok, self()}, State#state{users = NewUsers}}
    end;

handle_call({end_session, Username}, _From, State = #state{users = Users, rooms = Rooms}) ->
    case maps:find(Username, Users) of
        {ok, UserData} ->
            NewRooms = leave_all_rooms(Username, UserData, Rooms),
            NewUsers = maps:remove(Username, Users),
            {reply, ok, State#state{users = NewUsers, rooms = NewRooms}};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call(get_active_users, _From, State = #state{users = Users}) ->
    {reply, maps:to_list(Users), State};

handle_call(get_online_users, _From, State = #state{users = Users}) ->
    OnlineUsers = [{Username, UserData} || {Username, UserData} <- maps:to_list(Users)],
    {reply, {ok, OnlineUsers}, State};

handle_call({create_room, RoomName}, _From, State = #state{rooms = Rooms}) ->
    case maps:is_key(RoomName, Rooms) of
        true ->
            {reply, {error, room_exists}, State};
        false ->
            {ok, Pid} = chat_room:start_link(RoomName),
            NewRooms = maps:put(RoomName, Pid, Rooms),
            {reply, ok, State#state{rooms = NewRooms}}
    end;

handle_call({delete_room, RoomName}, _From, State = #state{users = Users, rooms = Rooms}) ->
    case maps:find(RoomName, Rooms) of
        {ok, RoomPid} ->
            gen_server:stop(RoomPid),
            UpdatedRooms = maps:remove(RoomName, Rooms),
            UpdatedUsers = maps:map(fun(_, UserData) ->
                UserRooms = maps:get(rooms, UserData, []),
                UpdatedUserRooms = lists:delete(RoomName, UserRooms),
                UserData#{rooms => UpdatedUserRooms}
            end, Users),
            {reply, ok, State#state{users = UpdatedUsers, rooms = UpdatedRooms}};
        error ->
            {reply, {error, room_not_found}, State}
    end;

handle_call({join_room, Username, RoomName}, _From, State = #state{users = Users, rooms = Rooms}) ->
    case {maps:find(Username, Users), maps:find(RoomName, Rooms)} of
        {{ok, UserData}, {ok, _RoomPid}} ->
            UserRooms = maps:get(rooms, UserData, []),
            NewUserData = UserData#{rooms => [RoomName | UserRooms]},
            NewUsers = maps:put(Username, NewUserData, Users),
            {reply, ok, State#state{users = NewUsers}};
        {{ok, _}, error} ->
            {reply, {error, room_not_found}, State};
        {error, _} ->
            {reply, {error, user_not_found}, State}
    end;

handle_call({leave_room, Username, RoomName}, _From, State = #state{users = Users, rooms = Rooms}) ->
    case {maps:find(Username, Users), maps:find(RoomName, Rooms)} of
        {{ok, UserData}, {ok, _RoomPid}} ->
            UserRooms = maps:get(rooms, UserData, []),
            NewUserRooms = lists:delete(RoomName, UserRooms),
            NewUserData = UserData#{rooms => NewUserRooms},
            NewUsers = maps:put(Username, NewUserData, Users),
            {reply, ok, State#state{users = NewUsers}};
        {{ok, _}, error} ->
            {reply, {error, room_not_found}, State};
        {error, _} ->
            {reply, {error, user_not_found}, State}
    end;

handle_call(get_rooms, _From, State = #state{rooms = Rooms}) ->
    {reply, {ok, maps:keys(Rooms)}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Helper functions
get_session_manager() ->
    global:whereis_name(?MODULE).

leave_all_rooms(Username, UserData, Rooms) ->
    UserRooms = maps:get(rooms, UserData, []),
    lists:foldl(fun(RoomName, AccRooms) ->
        chat_room:leave(RoomName, Username),
        AccRooms
    end, Rooms, UserRooms).