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
  odbc_query(Connection, "INSERT INTO arts (title, art) VALUES ('~w', '~w')"-[ETitle, EArt]),
  odbc_disconnect(Connection).

db_select(ArtList) :-
  odbc_connect('blog', Connection, []),
  odbc_query(Connection, "SELECT title, art FROM arts ORDER BY created DESC", 
    ArtList, [findall(row(Title, Art), row(Title, Art))]),  
  odbc_disconnect(Connection).

front_handler(Request) :-
  member(method(get), Request), !,
  render_page('', '', '').

front_handler(Request) :-
  member(method(post), Request), !,
  http_parameters(Request, [title(Title, [default('')]), art(Art, [default('')])]),
  ((string_length(Title, 0) ; string_length(Art, 0)) -> render_page(Title, Art, 'You need a title and art') 
                                                      ; db_insert(Title, Art), render_page('','','')).

render_page(Title, Art, Error) :-
  arts_html(ArtsHtml),
  reply_html_page([\['
    <title>Unit 3</title>
    <link rel="stylesheet" href="/styles/basic.css">']], [\['
    <form method="POST">
      <label for="title">Title</label>
      <input type="text" name="title" value="'], Title, \['">
      <label for="art">Art</label>
      <textarea name="art">'], Art, \['</textarea>
      <div class="error">'], 
        Error, \['
      </div>
      <button type="submit">Send your art</button>
    </form>
    <hr>'],
    \[ArtsHtml]]).
  
art_html(row(Title, Art), HtmlString) :-
  atomics_to_string(['<div class="post">\n<div class="post-title">\n', 
    Title, '\n</div>\n<pre class="post-content">\n', Art, '\n</pre>\n</div>\n'], '', HtmlString).

arts_html(Html) :-
  db_select(ArtList),
  maplist(art_html, ArtList, HtmlList),
  atomics_to_string(HtmlList, '', Html).
  

