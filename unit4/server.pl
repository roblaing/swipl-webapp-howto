:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_unix_daemon)).
:- use_module(library(http/http_files)).

:- initialization http_daemon.

:- http_handler(root(.),      http_reply_from_files('.', [indexes(['welcome.html'])]), [prefix]). 
:- http_handler(root(signup), signup_handler(Method), [method(Method)]).
:- http_handler(root(login), http_reply_from_files('.', [indexes(['signup-form.html'])]), [prefix]).
:- http_handler(root(logout), http_reply_from_files('.', [indexes(['signup-form.html'])]), [prefix]).
:- http_handler(root(request), show_request, []).
:- http_handler(root(check_name), check_name, []).
:- http_handler('/styles/basic.css', http_reply_from_files('.', [indexes(['./styles/basic.css'])]), [prefix]).
:- http_handler('/scripts/signup-form.js', http_reply_from_files('.', [indexes(['./scripts/signup-form.js'])]), [prefix]).

signup_handler(get, Request) :-
  %term_string(Request, String),
  member(cookie(Cookies), Request), !,
  term_string(Cookies, String),
  render_signup_form('', '', '', '', '', '', '', '', String).

signup_handler(get, Request) :-
  term_string(Request, String),
  render_signup_form('', '', '', '', '', '', '', '', String).
  
signup_handler(post, Request) :-
  term_string(Request, String),
  reply_html_page(
    [title('Signup'),
     link([rel('stylesheet'), href('/styles/basic.css')])],
    [p(String)]).    

render_signup_form(Username, UsernameError, Password, PasswordError, Verify, VerifyError, Email, EmailError, RequestString) :-
  reply_html_page(
    [title('Signup'),
     link([rel('stylesheet'), href('/styles/basic.css')])],
    [form([name('signup'), action=('/signup'), method('POST'), onsubmit('return validateForm()')],
      [h2('Signup'),
       div([label([for('username')], 'Username:'),
            input([type('text'), id('username'), name('username'), value(Username)]),
            span([class('error'), id('error_username')], UsernameError)]),
       div([label([for('password')], 'Password:'),
            input([type('password'), id('password'), name('password'), value(Password)]),
            span([class('error'), id('error_password')], PasswordError)]),
       div([label([for('verify')], 'Verify:'),
            input([type('password'), id('verify'), name('verify'), value(Verify)]),
            span([class('error'), id('error_verify')], VerifyError)]),
       div([label([for('email')], 'Email (optional):'),
            input([type('text'), id('email'), name('email'), value(Email)]),
            span([class('error'), id('error_email')], EmailError)]),
       input([type('hidden'), id('salt'), name('salt'), 
         value('Some big secret that nobody should be able to read, but unfortunately anybody can')]),
       div([class="button"], button([type('submit')], 'Subscribe'))]),
     p(RequestString),
     \['<script src="/scripts/signup-form.js"></script>']]). % Needed to handle hyphen in url

show_request(Request) :-
  term_string(Request, String),
  reply_html_page(
   [title('See Request')],
   [p(String)]).

name_in_database(Username) :-
  odbc_connect('blog', Connection, []),
  odbc_query(Connection, "SELECT name FROM users WHERE name = '~w'"-[Username], row(_)),  
  odbc_disconnect(Connection).

check_name(Request) :-
  member(method(post), Request), !,
  http_read_json_dict(Request, _{userName:NameIn}),
  (name_in_database(NameIn) -> NameOut = "Sorry, that user name is already taken" ; NameOut = NameIn),
  reply_json_dict(_{nameOk:NameOut}).

