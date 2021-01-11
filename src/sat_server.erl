-module(sat_server).

-behaviour(gen_server).

-import(solver, [solve/1]).

%% API
-export([start_link/1, start_link/0, get_count/0, stop/0, run/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 3547).
-record(state, {port, lsock, connections = 0}).

run() ->
    io:format("Server module~n").

% API functions
start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

start_link() ->
    start_link(?DEFAULT_PORT).

get_count() ->
    gen_server:call(?SERVER, get_count).

stop() ->
    gen_server:cast(?SERVER, stop).

% gen_server callbacks
init([Port]) ->
    {ok, LSock} = gen_tcp:listen(Port, [{active, true}, {reuseaddr, true}]),
    gen_server:cast(?SERVER, listen),
    {ok, #state{port = Port, lsock = LSock}, 0}.

handle_call(get_count, _From, State) ->
    {reply, {ok, State#state.connections}, State}.

handle_cast(listen, #state{lsock = LSock, connections = Connections} = State) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    Pid = spawn_link(client_server, start_link, [Sock]),
    gen_tcp:controlling_process(Sock, Pid),
    Connections = State#state.connections,
    gen_server:cast(?SERVER, listen),
    {noreply, State#state{connections = Connections + 1}};

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({tcp, Socket, String}, _State) ->
    {ok, Ts, _} = erl_scan:string(String ++ "."),
    {ok, Term} = erl_parse:parse_term(Ts),
    Result = solve(Term),
    gen_tcp:send(Socket, io_lib:fwrite("Res: ~w~n", [Result]));

handle_info({tcp_closed, _Socket}, #state{lsock = LSock} = State) ->
    {ok, _Sock} = gen_tcp:accept(LSock),
    {noreply, State};

handle_info(timeout, #state{lsock = LSock} = State) ->
    {ok, _Sock} = gen_tcp:accept(LSock),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
