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

-type couchbeam_server() :: any().
-type http_port() :: non_neg_integer().
-type user() :: binary() | string().

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

-spec connect() -> couchbeam_server().
connect() ->
    connect("localhost", 5984).

-spec connect(string(), http_port()) -> couchbeam_server().
connect(Host, Port) ->
    couchbeam:server_connection(Host, Port, "", []).

-spec fetch_user(couchbeam_server(), user()) -> [tuple()]
                                                    | {user_not_found,
                                                       not_in_view}
                                                    | {user_not_found,
                                                       {no_doc, binary()}}.
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

-spec fetch_org(couchbeam_server(), binary()) ->
    [tuple()]
        | {org_not_found, not_in_view}
        | {org_not_found, {no_doc, binary()}}.
fetch_org(Server, OrgName) when is_binary(OrgName) ->
    {ok, Db} = couchbeam:open_db(Server, ?user_db, []),
    {ok, View} = couchbeam:view(Db, {?mixlib_auth_org_design, "by_name"},
                                [{key, OrgName}]),
    case couchbeam_view:first(View) of
        {ok, {Row}} ->
            OrgDocId = ?gv(<<"id">>, Row),
            case couchbeam:open_doc(Db, OrgDocId) of
                {error, not_found} -> {org_not_found, {no_doc, OrgDocId}};
                {ok, {OrgDoc}} -> OrgDoc
            end;
        {ok, []} ->
            {org_not_found, not_in_view}
    end;
fetch_org(Server, OrgName) when is_list(OrgName) ->
    fetch_org(Server, list_to_binary(OrgName)).

-spec fetch_client(couchbeam_server(), [tuple()] | {org_not_found, _},
                   binary() | string()) ->
    [tuple()]
        | {client_not_found, not_in_view}
        | {client_not_found, {no_doc, binary()}}
        | {client_not_found, {org_not_found, _}}.
fetch_client(Server, Org, ClientName) when is_binary(ClientName), is_list(Org) ->
    ChefDb = "chef_" ++ ?gv(<<"guid">>, Org),
    {ok, Db} = couchbeam:open_db(Server, ChefDb, []),
    {ok, View} = couchbeam:view(Db, {?mixlib_auth_client_design, "by_clientname"},
                                [{key, ClientName}]),
    case couchbeam_view:first(View) of
        {ok, {Row}} ->
            ClientId = ?gv(<<"id">>, Row),
            case couchbeam:open_doc(Db, ClientId) of
                {error, not_found} -> {client_not_found, {no_doc, ClientId}};
                {ok, {ClientDoc}} -> ClientDoc
            end;
        {ok, []} ->
            {client_not_found, not_in_view}
    end;
fetch_client(Server, Org, ClientName) when is_list(ClientName), is_list(Org) ->
    fetch_client(Server, Org, list_to_binary(ClientName));
fetch_client(_Server, Reason={org_not_found, _}, _ClientName) ->
    {client_not_found, Reason}.

-spec bulk_get(couchbeam_server(), string(), [binary()]) ->
    [[tuple()]].
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


     {"fetch_org not found",
      fun() ->
              ?assertEqual({org_not_found, not_in_view},
                           chef_otto:fetch_org(S, <<"no-such-org">>))
              % FIXME: how can we test the case when org is in view,
              % but not found in the db.  Need to either manipulate a
              % test couchdb or introduce some mocks.
      end
     },

     {"fetch_client",
      fun() ->
              Org = chef_otto:fetch_org(S, <<"clownco">>),
              Client = chef_otto:fetch_client(S, Org, <<"clownco-validator">>),
              ?assertEqual(<<"clownco">>, ?gv(<<"orgname">>, Client))
      end
     },

     {"fetch_client no such client",
      fun() ->
              Org = chef_otto:fetch_org(S, <<"clownco">>),
              ?assertEqual({client_not_found, not_in_view},
                 chef_otto:fetch_client(S, Org, <<"not-a-known-client">>))
      end
     },

     {"fetch_client with missing org",
      fun() ->
              Org = chef_otto:fetch_org(S, <<"no-such-org">>),
              ?assertEqual({client_not_found, {org_not_found, not_in_view}},
                 chef_otto:fetch_client(S, Org, <<"not-a-known-client">>))
      end
     }



     ].
    
-endif.
