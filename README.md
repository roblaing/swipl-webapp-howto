# Writing a blog using SWI Prolog

These are notes I'm writing on how to use [SWI Prolog](http://www.swi-prolog.org/) to write web applications as I learn it to develop my strategy game playing website [newsgames.biz](http://www.newsgames.biz/).

My aim here is to gradually re-implement a [web development](https://eu.udacity.com/course/web-development--cs253) course I did a few years ago given by Reddid founder Steve Huffman which Udacity still offers for free.

Following the original course's basic outline, I plan to split the tutorial into about seven units which build on each other to create a fairly
fully featured blog which includes user authentication and storing comments in a database.

Whereas Huffman's course involved signing up for a free Google Appengine account offering a Python framework and a built in SQL server, 
I'm redoing it on a Linux localhost with SWI-Prolog talking to PostgreSQL via its ODBC package. 

My main objective is to provide some simple examples for my own reference and education, and I like Linux, Postgres, and SWI Prolog obviously.
If you prefer, say Windows and MySQL, hopefully it will only take you a bit of googling to adapt these examples.

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

For this to work, you need a recent version of SWI Prolog (8.1.5 at time of writing). The older version installed by my Linux distribution didn't work, so I had to upgrade by compiling from source code to get this to succeed.

```prolog
:- http_handler(root(user/User), my_handler_code(User), []).

my_handler_code(User, Request) :-
  member(request_uri(URI), Request),
  reply_html_page([head({|html(User)||
      <meta charset="utf-8">
      <title>User</title>
      <link rel="stylesheet" href="/styles/basic.css">
    |})],
    [body({|html(User, URI)||
      <h1><span>User</span>&#39; Home Page</h1>
      <ol>
        <li><a href="/">Home</a></li>
        <li><a href="/about">About</a></li>
        <li><a href="URI">URI</a></li>
      </ol>
      <p><img src="/images/swipl.png" alt="SWI Prolog Logo"/></p>
    |})]).
```

Pointing your browser to <http://localhost:3030/user/Joe%20Blog> should bring up Joe Blog's Home Page.

### Quick note on quasiquoting

When I originally wrote the above handler, I hadn't encountered [quasiquoting](http://www.swi-prolog.org/pldoc/man?section=quasiquotations) and wrote it like so:

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

I much prefer keeping HTML as HTML instead of converting it into a prologish dialect of HTML.

## Unit 2

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

If a parameter is missing or does not fit into the type checks done, http_parameters throws a _400 Bad Request_ error. Instead of showing that, I've opted for [catch(:Goal, +Catcher, :Recover)](http://www.swi-prolog.org/pldoc/doc_for?object=catch/3) to return to the original form without any error messages or keeping the original data at this stage. 

I did this before discovering quasiquoting described above, so intend to come back and make it more elaborate in due course.

## Unit 3

This is work in progress...

This unit introduces an SQL database which SWI-Prolog communicates with via the [ODBC Interface](http://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/odbc.html%27)).

For Postgres (which I use) you need to have the [PostgreSQL ODBC driver](https://odbc.postgresql.org/) installed besides an ~/.odbc.ini file.
Details are at <http://www.unixodbc.org/odbcinst.html> where it explains how to set this up for alternatives to Postgres. The below example could be one of many stanzas in the .odbc.ini file for various databases, each referenced by SWI Prolog by whatever identifier you put in the heading between square brackets.

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

Assuming you're using Postgres and it is all set up nicely, follow the instructions at [Creating a Database](https://www.postgresql.org/docs/current/manage-ag-createdb.html) to install a new database called whatever you want to use as my_database_name (I suggest blog, same as the identifier in the .odbc.ini file). If you already have a database, you can skip this step and use it to store the tables coming up.

On most Linux systems, all that would be required are these two commands at the bash shell to get you into the Postgres shell: 
```bash
createdb my_database_name
psql -d my_database_name
```

```sql
CREATE TABLE IF NOT EXISTS links (
    id            SERIAL PRIMARY KEY,
    submitter_id  INTEGER REFERENCES submitters(id),
    submitted_time TIMESTAMP, 
    votes,
    title, 
    url
);
```
