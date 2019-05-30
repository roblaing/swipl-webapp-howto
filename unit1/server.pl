:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_unix_daemon)).

:- initialization http_daemon.

:- multifile http:location/3.
http:location(files, root(files), []).
user:file_search_path(folders, library('images/styles/scripts')).

:- http_handler(root(.),     http_reply_from_files('.', [indexes(['./index.html'])]), [prefix]).
:- http_handler(root(about), http_reply_from_files('.', [indexes(['./about.html'])]), [prefix]).
:- http_handler(files(.),    http_reply_from_files(folders, []), [prefix]).
:- http_handler(root(user/User), my_handler_code(User), []).

my_handler_code(User, Request) :-
  term_string(Request, String),
  reply_html_page(
     [title("~w's Home Page"-[User]),
      link([rel('stylesheet'), href('/styles/basic.css')])],
     [h1("~w's Home Page"-[User]),
      ol([li(a([href('/')], 'Home')),
          li(a([href('/about')], 'About'))]),
      p(String)]).

