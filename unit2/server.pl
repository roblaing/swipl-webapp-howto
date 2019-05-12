:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_unix_daemon)).
:- use_module(library(http/http_parameters)).

:- initialization http_daemon.

:- multifile http:location/3.
http:location(files, root(files), []).
user:file_search_path(folders, library('images/styles/scripts')).

:- http_handler(root(.),     http_reply_from_files('.', [indexes(['index.html'])]), [prefix]).
:- http_handler(files(.),    http_reply_from_files(folders, []), [prefix]).
:- http_handler(root(form),  form_handler, []).

form_handler(Request) :-
  % member(method(get), Request), !,
  catch(http_parameters(Request, 
      [month(Month, [oneof(['January','February','March','April','May','June','July','August','September','October','November','December'])]),
       day(Day, [between(1, 31)]),
       year(Year, [between(1890, 2030)])]),
    error(_, _),
    http_reply_from_files('.', [indexes(['index.html'])], Request)),
  reply_html_page([title('Birthday')],
     [p(['Month: ', Month, ' Day: ', Day, ' Year: ', Year])]).
   

