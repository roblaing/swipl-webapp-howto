:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_unix_daemon)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_parameters)).
:- use_module(library(sha)).

:- initialization(http_daemon, main).

:- multifile http:location/3.
:- dynamic   http:location/3.
http:location(images, root(images), []).
http:location(styles, root(styles), []).
http:location(scripts, root(scripts), []).

:- http_handler(root(.), welcome_or_login, []).
:- http_handler(root(login), login_handler(Method), [method(Method)]).
:- http_handler(root(logout), http_reply_from_files('.', [indexes(['./logout.html'])]), [prefix]).
:- http_handler(root(signup), signup_handler(Method), [method(Method)]).
:- http_handler(images(.), http_reply_from_files('./images', []), [prefix]).
:- http_handler(styles(.), http_reply_from_files('./styles', []), [prefix]).
:- http_handler(scripts(.), http_reply_from_files('./scripts', []), [prefix]).

welcome_or_login(Request) :-
  ( logged_in(Request, Name) -> 
    term_string(Request, String),
    render_welcome(Name, String)
  ; 
    http_redirect(see_other, root(login), Request)).
                            
login_handler(get, Request) :-
  (logged_in(Request, _Name) -> http_redirect(see_other, root(.), Request)
                              ; render_login_form(Request)).

login_handler(post, Request) :-
  (logged_in(Request, _Name) -> http_redirect(see_other, root(.), Request)
                              ; http_redirect(see_other, root(login), Request)).

signup_handler(get, Request) :-
  (logged_in(Request, _Name) -> http_redirect(see_other, root(.), Request)
  ; term_string(Request, String),
    render_signup_form('', '', '', '', String)).
  
signup_handler(post, Request) :-
  http_parameters(Request, [username(Name, []), email(Email, [])]),
  (name_in_db(Name) -> term_string(Request, String),
    render_signup_form(Name, 'Sorry, that user name is already taken', Email, '', String)
  ;
   member(cookie(Cookies), Request),
   member(user_id=UserId, Cookies),
   create_hash(UserId, Id),
   insert_user(Id, Name, Email),
   http_redirect(see_other, root(.), Request)).
     
render_welcome(Username, RequestString) :-
  reply_html_page(
    [title('Welcome ~w'-[Username]),
     link([rel('stylesheet'), href('/styles/basic.css')])],
    [div([class('login-area')],[Username, ' ', a([class('login-link'), href('/logout')],'Logout')]),
     h1('Welcome ~w'-[Username]),
     p(RequestString)]).
    
render_login_form(Request) :-
  term_string(Request, RequestString),
  ((member(cookie(Cookies), Request), member(user_id=_UserId, Cookies)) -> 
    LoginError = 'Sorry, that is not a valid login and password' ; LoginError = '' ),
  reply_html_page(
    [title('Login'),
     link([rel('stylesheet'), href('/styles/basic.css')])],
    [div([class('login-area')],[a([class('login-link'), href('/signup')],'Signup')]),
     form([name('login'), action=('/login'), method('POST'), onsubmit('return validateLoginForm()')],
      [h2('Login'),
       div([label([for('username')], 'Username:'),
            input([type('text'), id('username'), name('username')])]),
       div([label([for('password')], 'Password:'),
            input([type('password'), id('password'), name('password')])]),
       div([class('error'), id('login_error')], LoginError),
       input([type('hidden'), id('salt'), name('salt'), 
         value('Some big secret that nobody should be able to read, but unfortunately anybody can')]),
       div([class="button"], button([type('submit')], 'Login'))]),
     p(RequestString),
     script([src('/scripts/signup_form.js')],'')]).
  
render_signup_form(Username, UsernameError, Email, EmailError, RequestString) :-
  reply_html_page(
    [title('Signup'),
     link([rel('stylesheet'), href('/styles/basic.css')])],
    [div([class('login-area')],[a([class('login-link'), href('/login')],'Login')]),
     form([name('signup'), action=('/signup'), method('POST'), onsubmit('return validateSignupForm()')],
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
     script([src('/scripts/signup_form.js')],'')]).

sql_escape_single_quotes(StringIn, StringOut) :-
  split_string(StringIn, "'", "", List),
  atomics_to_string(List, "''", StringOut).
   
insert_user(Id, Name, Email) :-
  sql_escape_single_quotes(Name, EName),
  sql_escape_single_quotes(Email, EEmail),
  odbc_connect('blog', Connection, []),
  odbc_query(Connection, "INSERT INTO users (id, name, email) VALUES ('~w', '~w', '~w')"-[Id, EName, EEmail]),
  odbc_disconnect(Connection). 

name_in_db(Name) :-
  odbc_connect('blog', Connection, []),
  odbc_query(Connection, "SELECT name FROM users WHERE name = '~w'"-[Name], row(Name)),  
  odbc_disconnect(Connection).

create_hash(String, HexDigest) :-
  Salt = "Some very long randomly generated string",
  hmac_sha(Salt, String, HMAC, [algorithm(sha256)]),
  hash_atom(HMAC, HexDigest).

logged_in(Request, User) :-
  member(cookie(Cookies), Request),
  member(user_id=UserId, Cookies),
  create_hash(UserId, Id),
  odbc_connect('blog', Connection, []),
  odbc_query(Connection, "SELECT name FROM users WHERE id = '~w'"-[Id], row(User)),  
  odbc_disconnect(Connection).


