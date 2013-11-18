%%%-------------------------------------------------------------------
%%% @version 1.0.0
%%% @author erikh
%%% @copyright (C) 2013, Codemate
%%% @doc
%%% Sample erlang code to test blueprint extraction
%%% @end
%%%-------------------------------------------------------------------
%%% @@blueprint
%%% FORMAT: 1A
%%%
%%% # Gist Fox API
%%% Gist Fox API is a **pastes service** similar to [GitHub's Gist][http://gist.github.com].

-module(apiary_test).
-author("erikh").

-export([test/0]).

%% @doc
%% Makes a test call
%% @end
%% @@blueprint - Copied from Gist Fox API.md
%% # Gist Fox API Root {@module} [/]
%% Gist Fox API entry point.
%% 
%% This resource does not have any attributes. Instead it offers the initial API affordances in the form of the HTTP Link header and HAL links.
%% 
%% ## Retrieve Entry Point [GET]
%% @@end
%%
%% this is not part of the blueprint
%%
%% @@blueprint
%% 
%% + Response 200 (application/hal+json)
%%     + Headers
%% 
%%             Link: <http:/api.gistfox.com/>;rel="self",<http:/api.gistfox.com/gists>;rel="gists"
%% 
%%     + Body
%% 
%%             {
%%                 "_links": {
%%                     "self": { "href": "/" },
%%                     "gists": { "href": "/gists?{since}", "templated": true }
%%                 }
%%             }
test() ->
    ok.
