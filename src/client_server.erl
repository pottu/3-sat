-module(client_server).

-behaviour(gen_server).

-import(solver, [is_instance/1, solve/1]).

%% API
-export([start_link/0, start_link/1, stop/0, run/0]).

%% gen_server callbacks
-export([init/1, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {socket, result = no_result, solver = no_solver}).

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
    {ok, #state{socket = no_socket}}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({socket, Socket}, State) ->
  gen_tcp:send(Socket, io_lib:fwrite("~w~n", [ready])),
  erlang:send_after(10000, self(), pulse),
  {noreply, State#state{socket = Socket}};


% Handle client message.
handle_info({tcp, Socket, String}, State = #state{solver = Solver}) ->
    {ok, Ts, _} = erl_scan:string(String ++ "."),
    {ok, Term} = erl_parse:parse_term(Ts),
    % TODO: Check if Term is an instance.
    case {Term, Solver, is_instance(Term)} of
      {abort, Pid, _} when is_pid(Pid) -> 
        exit(Pid, kill),
        gen_tcp:send(Socket, io_lib:fwrite("aborted~n", [])),
        {noreply, State#state{solver = no_solver, result = aborted}};
      {Instance, no_solver, true} -> 
        Pid = spawn(solver, solve, [self(), Instance]),
        gen_tcp:send(Socket, io_lib:fwrite("trying~n", [])),
        {noreply, State#state{solver = Pid}};
      _Otherwise ->
        gen_tcp:send(Socket, io_lib:fwrite("ignored~n", [])),
        {noreply, State}
    end;

% Handle solver message.
handle_info({solution, Result}, State = #state{socket = Socket}) ->
  gen_tcp:send(Socket, io_lib:fwrite("~w~n", [Result])),
  {noreply, State#state{solver = no_solver, result = Result}};

% Handle closed client connection.
handle_info({tcp_closed, _Socket}, State) ->
    % TODO: stop() here
    {noreply, State};

% Handle pulse.
handle_info(pulse, State = #state{socket = Socket, result = Result, solver = Solver}) ->
  case Solver of
    no_solver -> 
      case Result of
        no_result -> ok;
        _Result   -> gen_tcp:send(Socket, io_lib:fwrite("~w~n", [Result]))
      end;
    _Pid -> gen_tcp:send(Socket, io_lib:fwrite("trying~n", []))
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
