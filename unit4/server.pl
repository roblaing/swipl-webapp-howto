:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_unix_daemon)).
:- use_module(library(http/http_files)).

:- initialization http_daemon.

:- multifile http:location/3.
http:location(files, root(files), []).
user:file_search_path(folders, library('images/styles/scripts')).
:- http_handler(files(.), http_reply_from_files(folders, []), [prefix]).

:- http_handler(root(.),      http_reply_from_files('.', [indexes(['welcome.html'])]), [prefix]). 
:- http_handler(root(signup), http_reply_from_files('.', [indexes(['signup-form.html'])]), [prefix]).

/*
login_handler(UserName, Request) :-
  phrase(html({|html(UserName, URI)||
<!DOCTYPE html>
<html lang="en">
  <head>
    <title>Login</title>
  </head>
  <body>
    <h2>Login</h2>
    <form method="post">
      <table>
        <tr>
          <td class="label">
            Username
          </td>
          <td>
            <input type="text" name="username" value="{{username}}">
          </td>
        </tr>

        <tr>
          <td class="label">
            Password
          </td>
          <td>
            <input type="password" name="password" value="">
          </td>
        </tr>
      </table>

      <div class="error">
        {{error}}
      </div>

      <input type="submit">
    </form>
  </body>

</html>
    |}), TokenizedHtml),
  format('Content-type: text/html~n~n'),
  print_html(TokenizedHtml).

front_handler(Request) :-
  term_string(Request, String),
  (http_session_data(visits(Visits0)) -> Visits is Visits0 + 1, 
                                         http_session_retractall(visits(_)) 
                                       ; Visits = 0),
  http_session_assert(visits(Visits)),
  reply_html_page([title('Simple Session Example')],
    [pre("You've been here ~w times."-[Visits]),
     p(String)]).
*/
