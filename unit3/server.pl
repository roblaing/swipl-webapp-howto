:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_unix_daemon)).
:- use_module(library(http/http_parameters)).
:- use_module(library(odbc)).

:- initialization http_daemon.

:- http_handler('/', front_handler, [methods([get, post])]).
:- http_handler('/styles/basic.css', http_reply_from_files('.', [indexes(['./styles/basic.css'])]), [prefix]).

sql_escape_single_quotes(StringIn, StringOut) :-
  split_string(StringIn, "'", "", List),
  atomics_to_string(List, "''", StringOut).

db_insert(Title, Art) :-
  sql_escape_single_quotes(Title, ETitle),
  sql_escape_single_quotes(Art, EArt),  
  odbc_connect('blog', Connection, []),
  odbc_query(Connection, "INSERT INTO arts (title, art) VALUES (\'~w\', \'~w\')"-[ETitle, EArt]),
  odbc_disconnect(Connection).

front_handler(Request) :-
  member(method(get), Request), !,
  render_page('', '', '').

front_handler(Request) :-
  member(method(post), Request), !,
  http_parameters(Request, [title(Title, [default('')]), art(Art, [default('')])]),
  ((string_length(Title, 0) ; string_length(Art, 0)) -> Error = 'You need a title and art' 
                                                      ; Error = '', db_insert(Title, Art)),
  render_page(Title, Art, Error).

render_page(Title1, Art1, Error) :-
  phrase(html({|html(Title1, Art1, Error)||
    <!DOCTYPE html>
    <html lang="en">
    <head>
      <meta charset="utf-8">
      <title>Unit 3</title>
      <link rel="stylesheet" href="/styles/basic.css">
    </head>
    <body>
      <form method="POST">
        <label for="title">Title:</label>
        <input type="text" name="title" value="Title1">
        <label for="art">Art:</label>
        <textarea name="art">Art1</textarea>
        <div class="error">Error</div>
        <button type="submit">Send your art</button>
      </form>
    </body>
    </html>
    |}), TokenizedHtml),
  format('Content-type: text/html~n~n'),
  print_html(TokenizedHtml).

