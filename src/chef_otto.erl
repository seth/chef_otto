-module(chef_otto).

-export([
         fetch_user/2,
         fetch_org/2,
         fetch_client/3,
         connect/0,
         connect/2,
         bulk_get/3,
         start0/0
         ]).

-define(gv(Key, PList), proplists:get_value(Key, PList)).

-define(user_db, "opscode_account").

-define(mixlib_auth_user_design,
        "Mixlib::Authorization::Models::User-e8e718b2cc7860fc5d5beb40adc8511a").

-define(mixlib_auth_org_design,
        "Mixlib::Authorization::Models::Organization-eed4ffc4a127815b935ff840706c19de").

-define(mixlib_auth_client_design,
        "Mixlib::Authorization::Models::Client-fec21b157b76e08b86e92ef7cbc2be81").


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

connect() ->
    connect("localhost", 5984).

connect(Host, Port) ->
    couchbeam:server_connection(Host, Port, "", []).

fetch_user(Server, User) when is_binary(User) ->
    {ok, Db} = couchbeam:open_db(Server, ?user_db, []),
    {ok, View} = couchbeam:view(Db, {?mixlib_auth_user_design, "by_username"},
                                [{key, User}]),
    case couchbeam_view:first(View) of
        {ok, {Row}} ->  
            UserId = ?gv(<<"id">>, Row),
            case couchbeam:open_doc(Db, UserId) of
                {error, not_found} -> {user_not_found, {no_doc, UserId}};
                {ok, {UserDoc}}    -> UserDoc
            end;
        {ok, []} ->
            {user_not_found, not_in_view}
    end;
fetch_user(Server, User) when is_list(User) ->
    fetch_user(Server, list_to_binary(User)).

fetch_org(Server, OrgName) when is_binary(OrgName) ->
    {ok, Db} = couchbeam:open_db(Server, ?user_db, []),
    {ok, View} = couchbeam:view(Db, {?mixlib_auth_org_design, "by_name"},
                                [{key, OrgName}]),
    case couchbeam_view:first(View) of
        {ok, {Row}} ->
            OrgDocId = ?gv(<<"id">>, Row),
            case couchbeam:open_doc(Db, OrgDocId) of
                {error, not_found} -> {org_not_found, OrgName, {no_doc, OrgDocId}};
                {ok, {OrgDoc}} -> OrgDoc
            end;
        {ok, []} ->
            {org_not_found, OrgName, not_in_view}
    end;
fetch_org(Server, OrgName) when is_list(OrgName) ->
    fetch_org(Server, list_to_binary(OrgName)).

fetch_client(Server, Org, ClientName) when is_binary(ClientName); is_list(Org) ->
    ChefDb = "chef_" ++ ?gv(<<"guid">>, Org),
    {ok, Db} = couchbeam:open_db(Server, ChefDb, []),
    {ok, View} = couchbeam:view(Db, {?mixlib_auth_client_design, "by_clientname"},
                                [{key, ClientName}]),
    case couchbeam_view:first(View) of
        {ok, {Row}} ->
            ClientId = ?gv(<<"id">>, Row),
            case couchbeam:open_doc(Db, ClientId) of
                {error, not_found} -> {client_not_found, ClientName, {no_doc, ClientId}};
                {ok, {ClientDoc}} -> ClientDoc
            end;
        {ok, []} ->
            {client_not_found, ClientName, not_in_view}
    end;
fetch_client(Server, Org, ClientName) when is_list(ClientName); is_list(Org) ->
    fetch_client(Server, Org, list_to_binary(ClientName));
fetch_client(Server, Reason={org_not_found, OrgName, Why}, ClientName) ->
    {client_not_found, ClientName, Reason}.

bulk_get(Server, DbName, Ids) ->
    {ok, Db} = couchbeam:open_db(Server, DbName, []),
    {ok, View} = couchbeam:all_docs(Db, [{keys, Ids}, {include_docs, true}]),
    DocCollector = fun({Row}, Acc) ->
                           {Doc} = ?gv(<<"doc">>, Row),
                           [Doc|Acc]
                   end,
     couchbeam_view:fold(View, DocCollector).

start0() ->
    application:start(sasl),
    application:start(crypto),
    application:start(ibrowse),
    application:start(couchbeam),
    {ok, chef_otto_start}.


-ifdef(TEST).
otto_integration_test_() ->
    {ok, chef_otto_start} = chef_otto:start0(),
    S = chef_otto:connect(),
    [{"fetch_user found",
      fun() ->
              Got = chef_otto:fetch_user(S, "clownco-org-admin"),
              ?assertEqual(<<"ClowncoOrgAdmin">>,
                           ?gv(<<"display_name">>, Got))
      end},

     {"fetch_user not found",
      fun() ->
              ?assertEqual({user_not_found, not_in_view},
                           chef_otto:fetch_user(S, "fred-is-not-found"))
      end},

     {"fetch_org",
      fun() ->
              Org = chef_otto:fetch_org(S, <<"clownco">>),
              ?assertEqual(<<"clownco-validator">>,
                           ?gv(<<"clientname">>, Org))
      end
     },

     {"fetch_client",
      fun() ->
              Org = chef_otto:fetch_org(S, <<"clownco">>),
              Client = chef_otto:fetch_client(S, Org, <<"clownco-validator">>),
              ?assertEqual(<<"clownco">>, ?gv(<<"orgname">>, Client))
      end
     }

     ].
    
-endif.
