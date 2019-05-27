:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_unix_daemon)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_parameters)).
:- use_module(library(sha)).

:- initialization http_daemon.

:- http_handler(root(.), welcome_or_login, []).
:- http_handler(root(login), login_handler(Method), [method(Method)]).
:- http_handler(root(signup), signup_handler(Method), [method(Method)]).
:- http_handler(root(logout), http_reply_from_files('.', [indexes(['signup-form.html'])]), [prefix]).
:- http_handler(root(check_name), check_name, []).
:- http_handler('/styles/basic.css', http_reply_from_files('.', [indexes(['./styles/basic.css'])]), [prefix]).
:- http_handler('/scripts/signup-form.js', http_reply_from_files('.', [indexes(['./scripts/signup-form.js'])]), [prefix]).

logged_in(Request, User) :-
  member(cookie(Cookies), Request),
  member(user_id=UserId, Cookies),
  create_hash(UserId, Id),
  odbc_connect('blog', Connection, []),
  odbc_query(Connection, "SELECT name FROM users WHERE id = '~w'"-[Id], row(User)),  
  odbc_disconnect(Connection).

welcome_or_login(Request) :-
  logged_in(Request, Name) -> render_welcome(Name) 
                            ; login_handler(get, Request).

signup_handler(get, Request) :-
  term_string(Request, String),
  render_signup_form('', '', '', '', String).
  
signup_handler(post, Request) :-
  http_parameters(Request, [username(Name, []), email(Email, [])]),
  member(cookie(Cookies), Request),
  member(user_id=UserId, Cookies),
  create_hash(UserId, Id),
  insert_user(Id, Name, Email), 
  render_welcome(Name).
     
render_welcome(Username) :-
  reply_html_page(
    [title('Welcome ~w'-[Username]),
     link([rel('stylesheet'), href('/styles/basic.css')])],
    [h1('Welcome ~w'-[Username])]).
  
render_signup_form(Username, UsernameError, Email, EmailError, RequestString) :-
  reply_html_page(
    [title('Signup'),
     link([rel('stylesheet'), href('/styles/basic.css')])],
    [form([name('signup'), action=('/signup'), method('POST'), onsubmit('return validateForm()')],
      [h2('Signup'),
       div([label([for('username')], 'Username:'),
            input([type('text'), id('username'), name('username'), value(Username)]),
            span([class('error'), id('error_username')], UsernameError)]),
       div([label([for('password')], 'Password:'),
            input([type('password'), id('password'), name('password')]),
            span([class('error'), id('error_password')], '')]),
       div([label([for('verify')], 'Verify:'),
            input([type('password'), id('verify'), name('verify')]),
            span([class('error'), id('error_verify')], '')]),
       div([label([for('email')], 'Email (optional):'),
            input([type('text'), id('email'), name('email'), value(Email)]),
            span([class('error'), id('error_email')], EmailError)]),
       input([type('hidden'), id('salt'), name('salt'), 
         value('Some big secret that nobody should be able to read, but unfortunately anybody can')]),
       div([class="button"], button([type('submit')], 'Subscribe'))]),
     p(RequestString),
     \['<script src="/scripts/signup-form.js"></script>']]). % script('/scripts/signup-form.js') seems to be broken by hyphen in url

sql_escape_single_quotes(StringIn, StringOut) :-
  split_string(StringIn, "'", "", List),
  atomics_to_string(List, "''", StringOut).
   
insert_user(Id, Name, Email) :-
  sql_escape_single_quotes(Name, EName),
  sql_escape_single_quotes(Email, EEmail),
  odbc_connect('blog', Connection, []),
  odbc_query(Connection, "INSERT INTO users (id, name, email) VALUES ('~w', '~w', '~w')"-[Id, EName, EEmail]),
  odbc_disconnect(Connection). 

name_in_database(Username) :-
  odbc_connect('blog', Connection, []),
  odbc_query(Connection, "SELECT name FROM users WHERE name = '~w'"-[Username], row(_)),  
  odbc_disconnect(Connection).

check_name(Request) :-
  member(method(post), Request), !,
  http_read_json_dict(Request, _{userName:NameIn}),
  (name_in_database(NameIn) -> NameOut = "Sorry, that user name is already taken" ; NameOut = NameIn),
  reply_json_dict(_{nameOk:NameOut}).

create_hash(String, HexDigest) :-
  Salt = "Some very long randomly generated string",
  hmac_sha(Salt, String, HMAC, [algorithm(sha256)]),
  hash_atom(HMAC, HexDigest).



