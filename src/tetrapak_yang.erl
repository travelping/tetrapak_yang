-module(tetrapak_yang).

-behaviour(tetrapak_task_boot).
-export([app/0, tasks/1]).

-behaviour(tetrapak_task).
-export([check/1, run/2]).
-export([format_error/1]).

-define(YANGDIR, tetrapak:path(filename:join(["src", "yang"]))).
-define(YANGBUILDDIR, tetrapak:path(filename:join(["src", "yang", "hrl"]))).
-define(YANGTESTDIR,  tetrapak:path("test")).
-define(YANGTESTHRL,  filename:join([?YANGTESTDIR, "*"])).
-define(YANGBUILDDIRTEST, tetrapak:path(filename:join(["test", "hrl"]))).

-define(BUILDTASKS, [{ "build:yang", ?MODULE, "compile yang models into Erlang headers", [{run_before, ["build:erlang"]}] },
                     { "clean:yang", ?MODULE, "Delete Erlang headers build from yang models."}]).
-define(TESTTASKS, [{ "test:build:yang", ?MODULE, "compile yang models into Erlang headers", [{run_before, ["test:ct"]}] },
                    { "clean:test:yang", ?MODULE, "Delete Erlang headers build from yang models." }]).

-include_lib("tetrapak/include/tetrapak.hrl").

app() ->
    application:load(tetrapak_yang),
    YangSrc = filelib:is_dir(?YANGDIR) orelse test_yangs_exists(),
    {YangSrc, YangSrc}.

tasks(tasks) ->
    BuildTasks = task_def(filelib:is_dir(?YANGDIR), ?BUILDTASKS),
    TestTasks  = task_def(test_yangs_exists(), ?TESTTASKS),
    BuildTasks ++ TestTasks;

tasks(_) ->
    [].

check("build:yang") ->
    % Please, provide an API for it, instead of update_cache
    Options = [{"build.erlc_options", [{i, ?YANGBUILDDIR} | tetrapak:config("build.erlc_options", [])]}],
    tetrapak_context:import_config(tetrapak_task:context(), tetrapak_task:directory(), #config{values = Options}),
    load_yang(check_yang_fun(?YANGDIR, ?YANGBUILDDIR));

check("test:build:yang") ->
    load_yang(check_yang_fun(?YANGTESTDIR, ?YANGBUILDDIRTEST));

check("clean:yang") ->
    tpk_util:check_files_exist(?YANGDIR, ".yang", ?YANGBUILDDIR, ".hrl");

check("clean:test:yang") ->
    tpk_util:check_files_exist(?YANGTESTDIR, ".yang", ?YANGBUILDDIRTEST, ".hrl").

run("build:yang", Files) ->
    run_foreach(fun yang_to_hrl/1, Files);

run("test:build:yang", Files) ->
    run_foreach(fun yang_to_hrl/1, Files);

run("clean:yang", HrlFiles) ->
    lists:foreach(fun ({_, Files}) -> tpk_file:delete(Files) end, HrlFiles);

run("clean:test:yang", HrlFiles) ->
    lists:foreach(fun ({_, Files}) -> tpk_file:delete(Files) end, HrlFiles).

%% ------------------------------------------------------------
%% -- Helpers
load_yang(Fun) ->
    case application:load(yang) of
        ok ->
            Fun();
        _ ->
            tetrapak:fail("can't load yang application")
    end.

check_yang_fun(In, Out) ->
    fun() ->
            filelib:ensure_dir(filename:join(Out, ".")),
            tpk_util:check_files_mtime(In, ".yang", Out, ".hrl")
    end.

task_def(Check, Definition) ->
    task_def(Check, Definition, []).

task_def(true, Definition, _Default) -> Definition;
task_def(false, _Definition, Default) -> Default.

test_yangs() ->
    filelib:wildcard(?YANGTESTHRL).

test_yangs_exists() ->
    length(test_yangs()) > 0.

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
