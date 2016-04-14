#!/usr/bin/env escript
%%! -pa ebin deps/ranch/ebin deps/goldrush/ebin deps/lager/ebin
%%
%% -*-coding: utf-8 -*-
%%


%% the script MUST contain function named `main` to start
main([]) ->
  io:format("Starting the application...~n"),
  RanchStatus = application:ensure_all_started(ranch),
  io:format("~tp~n", [RanchStatus]),
  SyntaxToolsStatus = application:ensure_all_started(syntax_tools),
  io:format("~tp~n", [SyntaxToolsStatus]),
  CompilerStatus = application:ensure_all_started(compiler),
  io:format("~tp~n", [CompilerStatus]),
  GoldRushStatus = application:ensure_all_started(goldrush),
  io:format("~tp~n", [GoldRushStatus]),
  LaggerStatus = application:ensure_all_started(lager),
  io:format("~tp~n", [LaggerStatus]),
  WallStatus = application:ensure_all_started(wall),
  io:format("~tp~n", [WallStatus]),
  io:format("done!~n").

