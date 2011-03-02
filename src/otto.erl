-module(otto).

-compile([export_all]).

-define(user_db, "opscode_account").
-define(mixlib_auth_user_design,
        "Mixlib::Authorization::Models::User-e8e718b2cc7860fc5d5beb40adc8511a").

fetch_user(Server, User) ->
    % FIXME: handle not found case, handle other errors
    {ok, Db} = couchbeam:open_db(Server, ?user_db, []),
    {ok, View} = couchbeam:view(Db, {?mixlib_auth_user_design, "by_username"},
                                [{key, User}]),
    {ok, {Row}} = couchbeam_view:first(View),
    UserId = proplists:get_value(<<"id">>, Row),
    {ok, {UserDoc}} = couchbeam:open_doc(Db, UserId),
    UserDoc.
