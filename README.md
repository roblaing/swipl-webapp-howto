# Writing a blog using SWI Prolog

These are notes I'm writing on how to use SWI Prolog to write web applications as I learn it to develop my strategy game playing website 
[newsgames.biz](http://www.newsgames.biz/).

My aim here is to gradually re-implement a web development course I did a few years ago given by Reddid founder Seve Huffman via Udacity. 
(Unfortunately when I looked, Huffman's excellent though now slightly out-dated course appeared to be gone).

Following the original course's basic outline, I plan to split the tutorial into about seven units which build on each other to create a fairly
fully featured blog which includes user authentication and storing comments in a database.

Whereas HUffman's course involved signing up for a free Google Appengine account offering a Python framework and a built in SQL server, 
I'm redoing it on a Linux localhost with SWI-Prolog talking to PostgreSQL via its ODBC package. 

My main objective is to provide some simple examples for my own reference and education, and I like Linux, Postgres, and SWI Prolog obviously.
If you prefer, say, Windows and MySQL hopefully it will only take you a bit of googling to adapt these examples.

## Unit 1

Unit 1 in the original course was devoted to explaining the basics of HTML, which I assume readers are familiar with &mdash; if not I recommend Mozilla's [Getting started with the Web](https://developer.mozilla.org/en-US/docs/Learn/Getting_started_with_the_web) &mdash; so I'll kick-off with basics of a SWI Prolog webserver.

My directory structure has a SWI Prolog file called server.pl which assumes html files are in the same directory while css files are in a subdirectory called styles, photos and graphics are in a subdirectory called images, and Javascript files in a subdirectory called scripts.

```
unit1
├── server.pl
├── index.html
├── about.html
├── images
│   └── swipl.png
├── scripts (empty in Unit 1)
└── styles
    └── basic.css
```

The server is started with, say, 
```bash
swipl server.pl --port=3030 --pidfile=http.pid
``` 
and stopped with 
```bash
kill $(cat http.pid)
```
and accessed by pointing your browser to <http://localhost:3030/>.

### Handlers

At the heart of the various web frameworks I've encountered over the years (Zope, Ruby on Rails, Django...) is parsing the directory structure
of incoming URLs into commands for the server to respond to. Pattern matching is one of Prolog's strongest suits, making slicing up URLs and dispatching
requests to the relevant handler shorter and sweeter than any of the _regex_ languages I've wrestled with.

The key clause here is [http_handler(+Path, :Closure, +Options)](http://www.swi-prolog.org/pldoc/doc_for?object=http_handler/3), and I've included
a couple of ways to use it in the introductory example.

Nearly every website wants to respond with a file called index.html if a browser is pointed at its root directory. SWI Prolog achieves this with one
line of code.

```prolog
:- http_handler(root(.), http_reply_from_files('.', []), [prefix]).
```

Borrowing [The Zen of Python](https://www.python.org/dev/peps/pep-0020/) rule 2 &mdash; explicit is better than implicit &mdash; 
I've expanded that because it took me a while to figure out how to write handlers for html files other than index.html.

```prolog
:- http_handler(root(.),     http_reply_from_files('.', [indexes(['index.html'])]), [prefix]).
:- http_handler(root(about), http_reply_from_files('.', [indexes(['about.html'])]), [prefix]).
```

Besides html files, the server also needs to handle associated MIME types, and SWI Prolog achieves that (in the directory structure explained
above) with a few lines of additional code.

```prolog
:- multifile http:location/3.
http:location(files, root(files), []).
user:file_search_path(folders, library('images/styles/scripts')).
:- http_handler(files(.), http_reply_from_files(folders, []), [prefix]).
```
I've included one _dynamic_ handler in the initial example which reads a directory name as a variable and responds programatically
with SWI Prolog's html generating system [html_write](http://www.swi-prolog.org/pldoc/man?section=htmlwrite).

```prolog
:- http_handler(root(user/User), my_handler_code(User), []).

my_handler_code(User, Request) :-
   member(request_uri(URI), Request),
   reply_html_page(
     [title([User, "'s Home Page"])],
	 [h1([User, "'s Home Page"]),
	  p('Hello ~w!'-[User]),
      p('uri ~w'-[URI])]).
```

Pointing your browser to <http://localhost:3030/user/Joe Blog> should bring up Joe Blog's Home Page.

## Unit 2

Is work in progress...

