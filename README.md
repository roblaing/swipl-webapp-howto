# Writing a blog using SWI Prolog

*By Robert “Joe Blog” Laing*

> “You think you know when you learn, are more sure when you can write, even more when you can teach, but certain when you can program.” ― [Alan Perlis](http://www.cs.yale.edu/homes/perlis-alan/quotes.html)

These are notes I'm writing on how to use [SWI Prolog](http://www.swi-prolog.org/) to write web applications as I discover it to develop my strategy game playing website [newsgames.biz](http://www.newsgames.biz/).

My aim here is to gradually re-implement a [web development](https://eu.udacity.com/course/web-development--cs253) course I did a few years ago given by Reddid founder Steve Huffman which Udacity still offers for free.

Following the original course's basic outline, I plan to split the tutorial into about seven units which build on each other to create a fairly
fully featured blog which includes user authentication and storing comments in a database.

Whereas Huffman's course involved signing up for a free Google Appengine account offering a Python framework and a built in SQL server, 
I'm redoing it on a Linux localhost with SWI-Prolog talking to PostgreSQL via its ODBC package. 

My main objective is to provide some simple examples for my own reference and education, and I like Linux, Postgres, and SWI Prolog obviously.
If you prefer, say Windows and MySQL, hopefully it will only take you a bit of googling to adapt these examples.

## Unit 1

> “When one teaches, two learn.” ― Robert Heinlein

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

The key predicate here is [http_handler(+Path, :Closure, +Options)](http://www.swi-prolog.org/pldoc/doc_for?object=http_handler/3), and I've included
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
```

You need a recent version of SWI Prolog (8.1.5 at time of writing) for the "directory as variable" technique to work. The older version installed by my Linux distribution gave an error, so I had to upgrade by compiling from source code to get this to succeed.

If everything is working, pointing your browser to <http://localhost:3030/user/Joe%20Blog> should bring up Joe Blog's Home Page.

### Generating HTML programmatically

SWI Prolog's [html DCG grammar](http://www.swi-prolog.org/pldoc/doc_for?object=html//1) offers many ways to generate HTML, and the way I'm doing it in this tutorial is fairly long winded &mdash; controlling the entire HTML template myself &mdash; because I'm ideologically opposed to the fashion in web application frameworks of hiding the underlying HTML from users, thereby creating monolithic, unportable, and unmaintainable content management systems.

The final argument passed by the closure, [Request](http://www.swi-prolog.org/pldoc/man?section=request), is the HTTP message presented as a Prologish list of *key(Value)* terms whose values can be extracted with clauses such as ```member(request_uri(URI), Request).``` I'll go into more details in Unit 4 when we need read cookies from the HTTP message.

I only discovered [quasiquoting](http://www.swi-prolog.org/pldoc/man?section=quasiquotations) while researching this tutorial, and rewrote my handler
to use it.

```prolog
my_handler_code(User, Request) :-
  member(request_uri(URI), Request),
  phrase(html({|html(User, URI)||
    <!DOCTYPE html>
    <html lang="en">
    <head>
      <meta charset="utf-8">
      <title>User</title>
      <link rel="stylesheet" href="/styles/basic.css">
    </head>
    <body>
      <h1><span>User</span>&#39;s Home Page</h1>
      <ol>
        <li><a href="/">Home</a></li>
        <li><a href="/about">About</a></li>
        <li><a href="URI">URI</a></li>
      </ol>
      <p><img src="/images/swipl.png" alt="SWI Prolog Logo"/></p>
    </body>
    </html>
    |}), TokenizedHtml),
  format('Content-type: text/html~n~n'),
  print_html(TokenizedHtml).
```

In my ignorance before converting to quasiquoting, I wrote this handler in a much simpler and shorter way.

```prolog
my_handler_code(User, Request) :-
   member(request_uri(URI), Request),
   reply_html_page(
     [title("~w's Home Page"-[User]),
      link([rel='stylesheet', href='/styles/basic.css'])],
     [h1("~w's Home Page"-[User]),
      p('Hello ~w!'-[User]),
      p('uri ~w'-[URI])]).
```
If you're not an HTML purist, that may be an easier route.

Besides quasiquotes, another way to keep HTML more legible and maintable in [reply_html_page(:Head, :Body)](http://www.swi-prolog.org/pldoc/doc_for?object=reply_html_page/2) is to use ```\['HTML code here...']``` syntax which I've used in Module 3 to render a list of ASCII art from a database created by iteration (which I couldn't figure out how to insert as a value acceptable in quasiquoting). 

Using single quotes means no need to escape the double quotes used to surround HTML attribute values, and SWI Prolog allows linebreaks in quotes. So instead of using index.html to render the home page, it could be done like this:

```prolog
:- http_handler(root(.), front_handler, []).

front_handler(_Request) :-
  reply_html_page([\['
    <title>Home Page</title>
    <link rel="stylesheet" href="/styles/basic.css">
    ']],
    [\['
    <h1>Home Page</h1>
    <ol>
      <li><a href="/about">About</a></li>
    </ol>
    <p><img src="/images/swipl.png" alt="SWI Prolog Logo"/></p>
    ']]).
```

## Unit 2

> “If you want to master something, teach it.” ― Richard Feynman

Here I introduce SWI Prolog's predicate for handling user input sent to the server from an HTML form, [http_parameters(+Request, ?Parameters)](http://www.swi-prolog.org/pldoc/doc_for?object=http_parameters/2). Besides making it easy to toggle between GET and POST, it also offers various ways to validate the incoming data.

The original Udacity course I'm using as a template devoted a fair amount of time going through check boxes, radio buttons, drop down menus and HTML's many other form elements. I personally can't remember all this stuff and just look it up when needed, so again recommend [Mozilla's tutorial](https://developer.mozilla.org/en-US/docs/Learn/HTML/Forms/Your_first_HTML_form) for anyone wanting a refresher.

The salient point is the browser sends a list of key=value pairs to the server &mdash; clearly visible in the URL if the method is GET, slightly less visible in the HTTP message if the method is POST &mdash; for the web application to use as arguments in functions.

Since GET is more visible, and therefore easier to debug, I'm going to use it in the introductory example. If you want to switch to POST, all that is required is editing the method attribute in a line in index.html from:
```html
<form name="birthday" action="/form" method="GET" onsubmit="return validateForm()">
```
to
```html
<form name="birthday" action="/form" method="POST" onsubmit="return validateForm()">
```

On the server side, I've kept the handler agnostic in this simple example, though it probably is a good idea to add a clause ```member(method(get), Request)``` or ```member(method(post), Request)``` to dissuade hackers from trying methods on your code you didn't foresee. I've included where this would go in server.pl as a comment.

I've redone Huffman's example of creating a simple form which asks for a person's birthdate in US-style of month, day and year. To avoid a long digression into datetime programming, I've limited the example to some very rudimentary and inflexible checks on the validity of the input. Perhaps because his course is a bit old, Huffman did the validation on the server, getting it to rewrite the form with error messages. I've instead simply used some client-side Javascript to catch typos and give the user hints to problems before the form is sent to the server. 

While my Javascript code will prevent innocent typos getting transmitted, it won't catch malicious tampering of the input data which can easily be done by editing the URL when GET is used. This example uses a belt and braces approach of having both the client and server validate the user's input:

```prolog
form_handler(Request) :-
  catch(http_parameters(Request, 
      [month(Month, [oneof(['January','February','March','April','May','June','July','August','September','October','November','December'])]),
       day(Day, [between(1, 31)]),
       year(Year, [between(1890, 2030)])]),
    error(_, _),
    http_reply_from_files('.', [indexes(['index.html'])], Request)),
  reply_html_page([title('Birthday')],
    [p(['Month: ', Month, ' Day: ', Day, ' Year: ', Year])]).
```

If a parameter is missing or any of the validation tests fail, http_parameters throws a _400 Bad Request_ error. Instead of showing that, I've opted for [catch(:Goal, +Catcher, :Recover)](http://www.swi-prolog.org/pldoc/doc_for?object=catch/3) to return to the original form without any error messages or keeping the original data at this stage. 

I did this before discovering quasiquoting described above, so intend to come back and make it more elaborate in due course.

## Unit 3

This unit introduces an SQL database which SWI-Prolog communicates with via the [ODBC Interface](http://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/odbc.html%27)).

For Postgres (which I use) you need to have the [PostgreSQL ODBC driver](https://odbc.postgresql.org/) installed besides an ~/.odbc.ini file.
Details are at <http://www.unixodbc.org/odbcinst.html> where it explains how to set this up for alternatives to Postgres. The below example could be one of many stanzas in the ~/.odbc.ini file for various databases, each referenced by SWI Prolog by whatever identifier you put in the heading between square brackets. 

So in this example, SWI Prolog would get the username, password, database name etc from the example ~/.odbc.ini file below if told ```odbc_connect('blog', Connection, []),```...

```
[blog]
Description         = The Blog Example
Driver              = /usr/lib/psqlodbcw.so
Trace               = Yes
TraceFile           = sql.log
Database            = my_database_name
Servername          = localhost
UserName            = my_username
Password            = my_password
Port                = 5432
Protocol            = 10.6
ReadOnly            = No
RowVersioning       = No
ShowSystemTables    = No
ShowOidColumn       = No
FakeOidIndex        = No
ConnSettings        =
MaxVarcharSize      = 5000
Pooling             = Yes
```
The last two entries are things I've learnt through bitter experience. If MaxVarcharSize is left unset, ODBC defaults to 256 characters (the old Twitter rather than a blog site) and pooling needs to be set to yes if your site gets even slightly busy.

Assuming you're using Postgres and it is all set up nicely, follow the instructions at [Creating a Database](https://www.postgresql.org/docs/current/manage-ag-createdb.html) to install a new database called whatever you want to use as *my_database_name* (I suggest blog, same as the identifier in the .odbc.ini file). If you already have a database, you can skip this step and use it to store the arts table coming up.

On most Linux systems, all that would be required are these two commands at the bash shell to get you into the Postgres shell: 
```bash
createdb my_database_name
psql -d my_database_name
```

Not much knowledge of SQL is needed for the basic examples here, but I highly recommend Stanford University's [free online course](https://lagunita.stanford.edu/courses/DB/2014/SelfPaced/about) to anyone who wants to learn more about this very important subject. I've found the better I understand SQL, the better I understand how to use Prolog as a deductive database.

My way to create a table in Postgresql to follow Huffman's ASCII-chan example in Unit 3 looks like this. You can simply cut and paste it into the psql command line and then exit with ```\q```.

```sql
CREATE TABLE IF NOT EXISTS arts (
    id      SERIAL PRIMARY KEY,
    title   TEXT NOT NULL,
    art     TEXT NOT NULL,
    created TIMESTAMP DEFAULT current_timestamp
);
```

A snag the original course circumvented by using Google's GQL database had me cursing SWI Prolog until I realised it was actually SQL's fault: you can't use single quotes within SQL text unless they are escaped with another single quote. The test ASCII art I used, obtained from <https://www.asciiart.eu/>, was this:

```
Little Linux penguin by Joan G. Stark

       .---.
      /     \
      \.@-@./
      /`\_/`\
     //  _  \\
    | \     )|_
   /`\_`>  <_/ \
jgs\__/'---'\__/
```

This little Linux penguin caused server.pl to keep barfing ```odbc: state 42601: error: unterminated quoted string at or near...``` until I eventually figured out I needed a helper predicate to check the input text for single quotes, and if so double them.

```prolog
sql_escape_single_quotes(StringIn, StringOut) :-
  split_string(StringIn, "'", "", List),
  atomics_to_string(List, "''", StringOut).

db_insert(Title, Art) :-
  sql_escape_single_quotes(Title, ETitle),
  sql_escape_single_quotes(Art, EArt),  
  odbc_connect('blog', Connection, []),
  odbc_query(Connection, "INSERT INTO arts (title, art) VALUES ('~w', '~w')"-[ETitle, EArt]),
  odbc_disconnect(Connection).
```

Though I wouldn't bet my life on it, running input text through sql_escape_single_quotes before inserting it into the database should hopefully secure the site against SQL injection attacks.

The prolog predicate to fetch all the ASCII art in the database, ordered by newness, looks like this:

```prolog
db_select(ArtList) :-
  odbc_connect('blog', Connection, []),
  odbc_query(Connection, "SELECT title, art FROM arts ORDER BY created DESC", 
    ArtList, [findall(row(Title, Art), row(Title, Art))]),  
  odbc_disconnect(Connection).
```

A lot of web application development boils down to list processing, which is again one of Prolog's strong suits &mdash; though not easily used because it's very different to conventional programming languages. I've written a tutorial at <https://swish.swi-prolog.org/p/yeQhnQSk.swinb> to get my own head around the basics of iterating through lists in Prolog, touching on its Definite Clause Grammar (DCG) notation used by SWI Prolog's html_write library. I'm far from a master at this stuff, but do know enough to get an idea of how powerful it can be.

To generate HTML from the list of row(Title, Art) clauses returned from db_select I've written a helper predicate, art_html, used as the first argument in [maplist(:Goal, ?List1, ?List2)](http://www.swi-prolog.org/pldoc/doc_for?object=maplist/3).

```prolog
art_html(row(Title, Art), HtmlString) :-
  atomics_to_string(['<div class="post">\n<div class="post-title">\n', 
    Title, '\n</div>\n<pre class="post-content">\n', Art, '\n</pre>\n</div>\n'], '', HtmlString).

arts_html(Html) :-
  db_select(ArtList),
  maplist(art_html, ArtList, HtmlList),
  atomics_to_string(HtmlList, '', Html).
```

The input form is a bit more convoluted than in Unit 2 since I'm using the home page to handle both get and post requests. This required me to set handlers to specific paths because the abstract folders path broke for some reason when I used the home page for both get and post. 

For the form, I reverted to Prolog's ```if -> then ; else``` syntax after getting exasperated catching various weird errors generated by http_parameters because I don't understand its options fully, but I generally got Huffman's ASCI-chan demo website to work pretty much like his.

## Unit 4

Work in progress...

This section deals with using cookies to authenticate users. Before stepping into this dangerous minefield, a quick digression into session basics.

SWI Prolog has a library for [HTTP session management](http://www.swi-prolog.org/pldoc/man?section=httpsession) which I've used to create a simple example of a counter of how often a person has visited the page, but note this resets itself to zero every time you close the page, or leave it unattended for a few minutes.

```prolog
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_unix_daemon)).
:- use_module(library(http/http_session)).

:- initialization http_daemon.

:- http_handler(root(.), front_handler, []).

front_handler(Request) :-
  term_string(Request, String),
  (http_session_data(visits(Visits0)) -> Visits is Visits0 + 1, 
                                         http_session_retractall(visits(_)) 
                                       ; Visits = 0),
  http_session_assert(visits(Visits)),
  reply_html_page([title('Simple Session Example')],
    [pre("You've been here ~w times."-[Visits]),
     p(String)]).
```

In the above little server.pl I also write out the somewhat mysterious Request list to show how SWI Prolog makes the HTTP message data sent by the browser available to the web application.

Note that the syntax used by http_session is one Prolog programmers will be familiar with to retract and assert terms in a clausal store. As can be seen by printing out the Request list, visits(Visits) is hidden with some encryption magic in a cookie called swipl_session.

This is all I'm going to say about SWI Prolog's http_session library, because I'm going to use client-side Javascript to write a cookie storing a hash of the user's login and password which tells the server all it needs to know. 

The way I've done it, a password is never sent to the server. If a user logs in on a different computer or the cookie has expired, the same login, password, and hash function will recreate the same cookie &mdash; so there's no reason for servers to ever see or store passwords at all, let alone in plain text, an elementary part of online security which a shocking number of big corporations fail to grasp.

For a blog, users typically don't want to enter their login and password every time, so the cookie can be made persistant.

As usual, I turned to Mozilla for help on Javascript's [Subtle​Crypto​.digest()](https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto/digest), and [Document​.cookie](https://developer.mozilla.org/en-US/docs/Web/API/Document/cookie) to write my [script](https://github.com/roblaing/swipl-webapp-howto/blob/master/unit4/scripts/signup-form.js). 

Though this hides your login and password from anyone who can legitimately or illegitimately see your ID on the server, if this hash was the same as the cookie they could still set it in their browser to masquerade as you. To avoid that, web applications should add some secret "salt" and rehash the hash read from the browser cookie before using it as the user's ID in a database.

A way to do this in SWI Prolog is with the [SHA* Secure Hash Algorithms](http://www.swi-prolog.org/pldoc/man?section=sha) library.

```prolog
:- use_module(library(sha)).

create_hash(String, HexDigest) :-
  Salt = "Some very long randomly generated string",
  hmac_sha(Salt, String, HMAC, [algorithm(sha256)]),
  hash_atom(HMAC, HexDigest).
```

To be continued...



