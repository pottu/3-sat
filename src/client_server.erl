-module(client_server).

-behaviour(gen_server).

-import(solver, [is_instance/1, solve/1]).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_info/2, terminate/2, handle_call/3, handle_cast/2]).

-record(state, {socket = no_socket, msg = no_msg, solver = no_solver, timeout = no_timer}).

-define(CLIENT_TIMEOUT, 60000).
-define(PULSE_INTERVAL, 10000).

% API functions
start_link() ->
  gen_server:start_link(?MODULE, [], []).

% ---- gen_server callbacks --------------------------------
init([]) ->
  {ok, #state{}}.

% Handle client socket receival.
handle_info({socket, Socket}, State) ->
  Msg = io_lib:fwrite("ready~n", []),
  gen_tcp:send(Socket, Msg),
  erlang:send_after(?PULSE_INTERVAL, self(), pulse),
  TRef = erlang:send_after(?CLIENT_TIMEOUT, self(), client_timeout),
  {noreply, State#state{socket = Socket, msg = Msg, timeout = TRef}};

% Handle client message.
handle_info({tcp, Socket, String}, State = #state{solver = Solver, timeout = Timeout}) ->
  try
    {ok, Ts, _} = erl_scan:string(String ++ "."),
    {ok, Term} = erl_parse:parse_term(Ts),
    case {Term, Solver} of
      {abort, Pid} when is_pid(Pid) -> 
        exit(Pid, kill),
        Msg = io_lib:fwrite("aborted~n", []),
        gen_tcp:send(Socket, Msg),
        TRef = erlang:send_after(?CLIENT_TIMEOUT, self(), client_timeout),
        {noreply, State#state{solver = no_solver, msg = Msg, timeout = TRef}};
      {_Term, no_solver} -> 
        case is_instance(Term) of
          true ->
            Pid = spawn(solver, solve, [self(), Term]),
            Msg = io_lib:fwrite("trying~n", []),
            gen_tcp:send(Socket, Msg),
            erlang:cancel_timer(Timeout),
            {noreply, State#state{solver = Pid, msg = Msg, timeout = no_timer}}
        end
    end
  catch
    _:_ ->
      Msg2 = io_lib:fwrite("ignored~n", []),
      gen_tcp:send(Socket, Msg2),
      {noreply, State#state{msg = Msg2}}
  end;

% Handle solver message.
handle_info({solution, Result}, State = #state{socket = Socket}) ->
  Msg = io_lib:fwrite("~w~n", [Result]),
  gen_tcp:send(Socket, Msg),
  TRef = erlang:send_after(?CLIENT_TIMEOUT, self(), client_timeout),
  {noreply, State#state{solver = no_solver, msg = Msg, timeout = TRef}};

% Handle closed client connection.
handle_info({tcp_closed, _Socket}, State) ->
  {stop, shutdown, State};

% Handle closed client connection.
handle_info({tcp_error, _Socket, _Reason}, State) ->
  {stop, shutdown, State};

% Handle pulse.
handle_info(pulse, State = #state{socket = Socket, msg = Msg}) ->
  gen_tcp:send(Socket, Msg),
  erlang:send_after(?PULSE_INTERVAL, self(), pulse),
  {noreply, State};

% Handle timeout.
handle_info(client_timeout, State = #state{socket = Socket}) ->
  gen_tcp:send(Socket, io_lib:fwrite("timeout~n", [])),
  {stop, shutdown, State}.

terminate(_Reason, #state{solver = Solver}) ->
  case is_pid(Solver) of
    true -> exit(Solver, kill);
    _    -> ok
  end.

handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast(_Request, State) ->
  {noreply, State}.
