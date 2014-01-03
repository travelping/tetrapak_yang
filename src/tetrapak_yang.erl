-module(tetrapak_yang).

-behaviour(tetrapak_task_boot).
-export([app/0, tasks/1]).

-behaviour(tetrapak_task).
-export([check/1, run/2]).
-export([format_error/1]).

-define(YANGDIR, tetrapak:path("src/yang/")).
-define(YANGBUILDDIR, tetrapak:path("src/yang/build/")).

app() ->
    application:load(tetrapak_yang),
    YangSrc = filelib:is_dir(?YANGDIR),
    {YangSrc, YangSrc}.

tasks(tasks) ->
    [
      { "build:yang", ?MODULE, "compile yang models into Erlang headers", [{run_before, ["build:erlang"]}] },
      { "clean:yang", ?MODULE, "Delete Erlang headers build from yang models." }
    ];

tasks(_) ->
    [].

check("build:yang") ->
    case application:load(yang) of
        ok ->
            filelib:ensure_dir(filename:join(?YANGBUILDDIR, ".")),
            tpk_util:check_files_mtime(?YANGDIR, ".yang", ?YANGBUILDDIR, ".hrl");
        _ ->
            {done, [{modules, []}]}
    end;

check("clean:yang") ->
    tpk_util:check_files_exist(?YANGDIR, ".yang", ?YANGBUILDDIR, ".hrl").

run("build:yang", Files) ->
    run_foreach(fun yang_to_hrl/1, Files);

run("clean:yang", HrlFiles) ->
    lists:foreach(fun ({_, Files}) -> tpk_file:delete(Files) end, HrlFiles).

%% ------------------------------------------------------------
%% -- Helpers
run_foreach(Function, List) ->
    Res = lists:foldl(fun (Item, DoFail) ->
                              case Function(Item) of
                                  ok    ->
                                      DoFail;
                                  error ->
                                      true
                              end
                      end, false, List),
    if Res  -> tetrapak:fail("compilation failed");
       true -> ok
    end.

yang_to_hrl({In, Out}) ->
    BaseDir = tetrapak:dir(),
    io:format("Compiling ~s~n", [tpk_file:relative_path(In, BaseDir)]),
    case yang:deep_parse_file(In) of
        {ok, Yang} ->
            Ts = yang:typespec(Yang),
            file:write_file(Out, yang_typespec:hrl(Ts));
        {error, Error} ->
            DisplayPath = tpk_file:relative_path(In, BaseDir),
            tpk_util:show_error_info(DisplayPath, "Error:", {?MODULE, Error})
    end.

format_error({_Line, Fmt, Error}) ->
    io_lib:format(Fmt, Error).
