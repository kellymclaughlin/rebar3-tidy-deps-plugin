-module(rebar_prv_tidy_deps).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, tidy_deps).
-define(DEPS, [app_discovery]).
-define(DEBUG(Str, Args), rebar_log:log(debug, Str, Args)).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
                                 {name, ?PROVIDER},
                                 {module, ?MODULE},
                                 {bare, true},
                                 {deps, ?DEPS},
                                 {example, "rebar3 compile"},
                                 {opts, []},
                                 {short_desc, short_desc()},
                                 {desc, desc()}]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {ok, untidy_profiles(untidy_default_deps(State))}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Internal functions
%% ===================================================================

short_desc() ->
    "Plugin to enable concise deps specification for GitHub Erlang projects".

desc() ->
    io_lib:format(
      "Plugin to enable concise deps specification for common case Erlang projects"
      "~n"
      "Many Erlang projects reside on GitHub and many have a "
      "fair amount of common information in their dependency specifications. "
      "This plugin is designed to allow a more clear and concise specification "
      "of dependencies that meet the following criteria:~n"
      "    1. Reside in a git repo on github (public or private repo)~n"
      "    2. Use \".*\" as the version regex~n"
      "    3. Do not employ any other dependency options such as 'raw'~n"
      "~n"
      "For a dependency that meets these criteria this plugin allows users to "
      "specify dependencies using the following alternative syntax:~n"
      "~n"
      "    {mydep, github, \"kellymclauglin/mydep.git\", {tag, \"1.0.1\"}}~n"
      "~n"
      "For private repos the syntax is similar:~n"
      "~n"
      "    {myprivatedep, github_private, \"kellymclauglin/mydep.git\", {tag, \"2.0.2\"}}~n"
      "~n"
      "A version regex of \".*\" is used and the repo name is appended to the "
      "\"git://github.com/\" URL or \"git@github.com:\" in the case of a private repo."
      "Configuration:"
      "~n"
      "Configure the plugin and and set it as a pre hook for the app_discovery and install_deps providers "
      "by adding the following to the rebar.config file:~n"
      "~n"
      "{plugins, [~n"
      "    {plugin_name, \".*\", {git, \"git://github.com/kellymclaughlin/rebar_prv_tidy_deps.git\", {tag, \"0.0.1\"}}}~n"
      "]}.~n"
      "{provider_hooks, [{pre, [{app_discovery, tidy_deps}, {install_deps, tidy_deps}]}]}.~n",
      []).

-spec untidy_default_deps(rebar_state:t()) -> rebar_state:t().
untidy_default_deps(State) ->
  Deps = rebar_state:get(State, {deps, default}, []),
  rebar_state:set(State, {deps, default}, untidy_deps(Deps)).

-spec untidy_deps([tuple()]) -> [tuple()].
untidy_deps(Deps) ->
    [begin
         case Dep of
             {Name, github, Repo, Vsn} ->
                 {Name, ".*", {git, "git://github.com/" ++ Repo, Vsn}};
             {Name, github_private, Repo, Vsn} ->
                 {Name, ".*", {git, "git@github.com:" ++ Repo, Vsn}};
             _ ->
                 Dep
         end
     end || Dep <- Deps].

-spec untidy_profiles(rebar_state:t()) -> rebar_state:t().
untidy_profiles(State) ->
    Profiles = rebar_state:get(State, profiles, []),
    lists:foldl(fun untidy_profile/2, State, Profiles).

-spec untidy_profile({atom(), [tuple()]}, rebar_state:t()) -> rebar_state:t().
untidy_profile({ProfileName, Options}, State) ->
    case lists:keyfind(deps, 1, Options) of
        {deps, ProfileDeps}  ->
            UntidyDeps = untidy_deps(ProfileDeps),
            rebar_state:set(State, {deps, ProfileName}, UntidyDeps);
        false ->
            State
    end.
