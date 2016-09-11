-module(dreg).

-export([send/3]).

-type key() :: binary().
-type message() :: term().
-type when_not_found() :: fun((key()) -> pid()).

-export_type([key/0, message/0, when_not_found/0]).

%===============================================================================
% API functions
%===============================================================================

-spec send(
	Key :: key(),
	Message :: message(),
	Fun :: when_not_found()
) -> ok.

send(Key, Message, Fun) ->
	ok.

%===============================================================================
