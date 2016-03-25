-export([deliver/2, deliver/3]).

deliver(To, Data) ->
  wok_mail:deliver(?MODULE, To, Data).

deliver(To, Data, Options) ->
  wok_mail:deliver(?MODULE, To, Data, Options).

