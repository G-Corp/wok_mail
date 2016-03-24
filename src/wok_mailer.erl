-module(wok_mailer).

-callback from() -> string() | binary().
-callback subject() -> string() | binary().
-callback cc() -> [string()] | [binary()].
-callback bcc() -> [string()] | [binary()].
-callback templates() -> [{text|html, string()}].
-callback done(Response :: any()) -> any().
