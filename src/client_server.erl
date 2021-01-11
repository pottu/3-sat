-module(client_server).

-behaviour(gen_server).

-import(solver, [solve/1]).

%% API
-export([start_link/0, start_link/1, stop/0, run/0]).

%% gen_server callbacks
-export([init/1, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {socket, result = none, solver = none}).

run() ->
    io:format("Server module~n").

% API functions
start_link(Socket) ->
    gen_server:start_link(?MODULE, [Socket], []).

start_link() ->
  gen_server:start_link(?MODULE, [], []).

stop() ->
    gen_server:cast(?SERVER, stop).

% ---- gen_server callbacks --------------------------------
%init([Socket]) ->
%    gen_tcp:send(Socket, io_lib:fwrite("~w~n", [ready])),
%    {ok, #state{socket = Socket}}.

init([]) ->
    {ok, #state{socket = none}}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({socket, Socket}, State) ->
  gen_tcp:send(Socket, io_lib:fwrite("~w~n", [ready])),
  {noreply, State#state{socket = Socket}};

% Handle client abort message.
handle_info({tcp, Socket, "abort"}, State = #state{solver = Solver}) ->
  case Solver of
    none -> ok;
    Pid  -> exit(Pid, kill)
  end,
  gen_tcp:send(Socket, io_lib:fwrite("~w~n", [aborted])),
  {noreply, State#state{solver = none}};

% Handle client instance.
handle_info({tcp, Socket, String}, State) ->
    {ok, Ts, _} = erl_scan:string(String ++ "."),
    {ok, Term} = erl_parse:parse_term(Ts),
    % TODO: Check if Term is an instance.
    Instance = Term,
    Pid = spawn_link(solver, solve, [self(), Instance]),
    erlang:send_after(10000, self(), pulse),
    {noreply, State#state{solver = Pid}};

% Handle solver message.
handle_info({solution, Result}, State = #state{socket = Socket}) ->
  gen_tcp:send(Socket, io_lib:fwrite("~w~n", [Result])),
  {noreply, State#state{solver = none}};

% Handle closed client connection.
handle_info({tcp_closed, _Socket}, State) ->
    % TODO: stop() here?
    {noreply, State};

% Handle pulse.
handle_info(pulse, State = #state{socket = Socket, result = Result, solver = Solver}) ->
  case Solver of
    none -> gen_tcp:send(Socket, io_lib:fwrite("~w~n", [Result]));
    _Pid -> gen_tcp:send(Socket, io_lib:fwrite("~w~n", [trying]))
  end,
  erlang:send_after(10000, self(), pulse),
  {noreply, State};

handle_info(timeout, State) ->
    % TODO: stop() here?
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
