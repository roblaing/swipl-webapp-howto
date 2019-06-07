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

As this tutorial developed, it took a couple of digressions into providing tips to Prolog novices, which experienced Prolog programmers can simply skip.

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

I recently went through the process of placing a SWI Prolog powered site on the internet with its own domain name using [Digital Ocean](http://www.digitalocean.com/?refcode=a32a25b52821) where I picked Centos 7 for my virtual machine and there was a gotcha &mdash; whatever port number you pick will work locally, but nginx, Apache, or whatever reverse proxy won't be able to serve it without the following magic incantation as root user:

```semanage port -a -t http_port_t  -p tcp 3030```

Once the port permissions are right, for nginx, all that's needed is creating an /etc/nginx/conf.d/mydomain.conf file along these lines:

```
server {
  server_name www.mydomain.com mydomain.com;
  access_log /var/log/nginx/mydomain.access.log main;
  error_log /var/log/nginx/mydomain.error.log notice;
  location / {
    proxy_pass http://localhost:3030/;
  }
}
```

### Handlers

At the heart of the various web frameworks I've encountered over the years (Zope, Ruby on Rails, Django...) is parsing the directory structure
of incoming URLs into commands for the server to respond to. Pattern matching is one of Prolog's strongest suits, making slicing up URLs and dispatching
requests to the relevant handler shorter and sweeter than any of the _regex_ languages I've wrestled with.

The key predicate here is [http_handler(+Path, :Closure, +Options)](http://www.swi-prolog.org/pldoc/doc_for?object=http_handler/3), and I've included
a couple of ways to use it in the introductory example.

#### Howto read Prolog documentation

Experienced Prolog programmers tend to find it obvious that the [+, -, or ? prefixes](http://www.swi-prolog.org/pldoc/man?section=modes) to arguments in the documentation tells you if you are dealing with an input, an output, or a *bidirectional* argument. 

There are no output arguments (- prefixes) in http_handler, making it effectively a procedure rather than a function, but it does have a colon before the Closure, which is worth a digression since even intermediate Prolog programmers are likely to find it confusing. 

Something that tripped me up learning Prolog was that you need to think in terms of input and output arguments within relations &mdash; to borrow spreadsheet or database jargon, think of relations as rows and arguments as columns with known data (inputs) or as columns which need to be calculated (outputs)  &mdash; which is somewhat alien to most of us brought up on programming languages with functions which substitute themselves into a return value.

If your query is so specific it only returns one row, it is *det* in Prolog jargon. If your result produces several rows, it is [nondet](http://www.swi-prolog.org/pldoc/man?section=unitbox), and you'll either want to iterate through these multiple answers, or figure out the bug in your *pattern-action* predicates covered in Unit 2. Much of web development boils down to list processing, so iteration will come up a lot in this tutorial. Getting to grips with the many ways of iterating tends to be quite a hurdle for anyone learning Prolog, which I've covered in a separate [tutorial](https://swish.swi-prolog.org/p/yeQhnQSk.swinb).

##### What is :Closure?

Long story short, [closures](https://simple.wikipedia.org/wiki/Closure_(computer_science)) are predicates some of whose arguments magically vanish when they are used as arguments in other predicates, but through some conjuring trick, reappear where the given predicate is declared. 

Whatever predicate you put as http_handler's second (ie :Closure) argument in turn has a hidden final argument &mdash; conventionally called *Request* &mdash; which gets passed to your handler as its final argument. In the static files examples in the next section which use a library predicate, http_reply_from_files, we can ignore *Request*, but it plays a leading role in subsequent examples when we write our own handlers.

Closures will reappear in Unit 3 when I use maplist to iterate through a list returned from a database. Because predicates such as [maplist(:Goal, ?List1, ?List2)](http://www.swi-prolog.org/pldoc/doc_for?object=maplist/3) and [call(:Goal)](http://www.swi-prolog.org/pldoc/doc_for?object=call/1) assume the *vanished* argument will be at the end &mdash; and in the case of maplist will be preceeded by other missing arguments which will be used to fill in values read by iterating over one or more input lists &mdash; the order of arguments in Prolog predicates is not arbitrary. Personally, the only way I got to grasp this was through practice.

### Static Pages

Nearly every website wants to respond with a file called index.html if a browser is pointed at its root directory. SWI Prolog achieves this with one
line of code.

```prolog
:- http_handler(root(.), http_reply_from_files('.', []), [prefix]).
```

Borrowing [The Zen of Python](https://www.python.org/dev/peps/pep-0020/) rule 2 &mdash; explicit is better than implicit &mdash; 
I've expanded that because it took me a while to figure out how to write handlers for html files other than index.html. I've also changed ```'index.html'``` to ```'./index.html'``` so as to make it clear that if you wanted to keep your html files in a subdirectory called, say html, you could.

```prolog
:- http_handler(root(.),     http_reply_from_files('.', [indexes(['./index.html'])]), [prefix]).
:- http_handler(root(about), http_reply_from_files('.', [indexes(['./about.html'])]), [prefix]).
```

Besides html files, the server also needs to handle associated MIME types, and SWI Prolog achieves that (in the directory structure explained
above) with a few lines of additional code.

```prolog
:- multifile http:location/3.
http:location(files, root(files), []).
user:file_search_path(folders, library('images/styles/scripts')).
:- http_handler(files(.), http_reply_from_files(folders, []), [prefix]).
```

A nice thing about static files is you can add to your html, css and Javascript files and then test changes by simply refreshing your browser. Any changes you make to server.pl will require you to kill and restart the daemon to take effect.

### Dynamic Pages.

I've included one handler in the initial example which reads a directory name as a variable to show how to create permalinks later.

```prolog
:- http_handler(root(user/User), my_handler_code(User), []).
```

You need a recent version of SWI Prolog (8.1.5 at time of writing) for the "directory as variable" technique to work. The older version installed by my Linux distribution gave an error, so I had to upgrade by compiling from source code to get this to succeed.

A key point here is that while *Request* doesn't appear within *my_handler_code* where it is called by http_handler, it does appear as the final argument where *my_handler_code* is coded below. *Request* can be preceeded by any number of arguments. If it has no other arguments, by Prolog convention for functor/0 it is written in http_handler without brackets. 

Irrespective of the programming language you use for web development, accessing what's in the HTTP header remains fundamental, so I've used [term_string(?Term, ?String)](http://www.swi-prolog.org/pldoc/doc_for?object=term_string/2) to make *Request* viewable in the example webpage. We'll be doing this again in the coming units to see how form data, cookies etc are communicated to the web application by the browser.

```prolog
my_handler_code(User, Request) :-
  term_string(Request, String),
  reply_html_page(
     [title("~w's Home Page"-[User]),
      link([rel('stylesheet'), href('/styles/basic.css')])],
     [h1("~w's Home Page"-[User]),
      ol([li(a([href('/')], 'Home')),
          li(a([href('/about')], 'About'))]),
      p(String)]).
```


If everything is working, pointing your browser to <http://localhost:3030/user/Joe%20Blog> should bring up Joe Blog's Home Page.

#### Generating HTML programmatically

SWI Prolog offers many ways of generating HTML programmatically. I recommend Anne Ogborne's [tutorial](http://www.pathwayslms.com/swipltuts/html/index.html) for a more comprehensive overview.

Its [definite clause grammar (DCG) for html](http://www.swi-prolog.org/pldoc/doc_for?object=html//1) allows you to write your web pages in a prologish way, converting HTML 

*&lt;element attribute1="value1"...&gt;Content&lt;/element&gt;* tags to 

*element([attribute1('value1'),...], Content)* clauses, 

where Content could also be a list, and if there are no attributes the sole argument could be one string as in ```h1("~w's Home Page"-[User])``` above. It also lets you stick to HMTL using \\['HTML here'], or [quasiquoting](http://www.swi-prolog.org/pldoc/man?section=quasiquotations). 

My own bias is toward separation of concerns by keeping HTML as HTML, but SWI Prolog's main developer Jan Wielemaker provided a strong counter-argument in a [discussion](https://swi-prolog.discourse.group/t/yet-another-web-applications-tutorial/566/13) I started from which this tutorial grew:

> Handling statements from other languages as plain strings and manipulating them using simple string operations is one of the most well understood routes to security issues. It is very much common practice as it seems so easy. Still, don’t. The main design guideline for the web services and most other interaction with other languages through strings is to avoid this and assemble expressions in the target language from data structures using code that fully supports the target language syntax and thus properly escapes special characters.

Since this tutorial is more about Prolog than HTML, I'll set my bias aside and use the prolog-style encouraged by [reply_html_page(:Head, :Body)](http://www.swi-prolog.org/pldoc/doc_for?object=reply_html_page/2) rather than quasiquoting.

## Unit 2

This unit covers the basics of web forms, which SWI Prolog makes fairly easy with one predicate [http_parameters(+Request, ?Parameters)](http://www.swi-prolog.org/pldoc/doc_for?object=http_parameters/2) which can be used without modification for GET or POST, and allows you to validate or give default values to the incoming key=value list.

The original Udacity course I'm using as a template devoted a fair amount of time going through check boxes, radio buttons, drop down menus and HTML's many other form elements. I personally can't remember all this stuff and just look it up when needed, so again recommend [Mozilla's tutorial](https://developer.mozilla.org/en-US/docs/Learn/HTML/Forms/Your_first_HTML_form) for anyone wanting a refresher.

It's generally good webform design practice to give users hints what they did wrong and allow them to edit their previous attempt instead of forcing them to start afresh, and redirect to a page saying the system is happy once the form has been filled in correctly. 

I've redone Huffman's example of creating a simple form which asks for a person's birthdate in US-style of month, day and year. To avoid a long digression into datetime programming, I've limited the example to some very rudimentary and inflexible checks on the validity of the input.

If you point your browser to http://localhost:3030, it should bring up a blank form. If you fill in values that validate, the browser takes you to a welcome page. But if it's not happy with the values, the form remains the homepage with red error messages indicating which fields are missing or wrong. 

Typically, POST is the preferred method for submitted data, making it easier for the server to know if this is a fresh form because then the method is GET. But since I'm striving to keep this example method agnostic (GET is easier to understand and debug since the data is visible in the URL and HTTP header), I've added an additional test to the first form_handler predicate which renders the initial, blank form.

```prolog
form_handler(Request) :-
  memberchk(method(get), Request), 
  \+memberchk(search(_), Request),
  term_string(Request, String),
  render_form('', '', '', '', '', '', String).  
```

If you fill in some nonsense values and click the form's submit button, the URL should show something like ```http://localhost:3030/?month=Movember&day=50&year=1776``` and a new entry appears in the Request list looking something like ```search([month='Movember',day='50',year='1776'])```.

When this gets submitted to server.pl, the conditions won't satisfy the first *form_handler(Request)* because there is now a search(Something) in the Request, or you have changed the example to use POST as described below, so gets passed on to the second *form_handler(Request)* predicate.

```prolog
form_handler(Request) :-
  (memberchk(method(post), Request) ; memberchk(search(_), Request)),
  term_string(Request, String),
  http_parameters(Request,
    [month(Month, [default('')]),
     day(Day, [default('')]),
     year(Year, [default('')])]),
  validate_form(Month, Day, Year, MonthError, DayError, YearError),
  ((MonthError = '', DayError = '', YearError = '') ->
    reply_html_page(
      [title('Birthday'),
       link([rel('stylesheet'), href('/styles/basic.css')])],
      [h2('Thanks, ~w ~w ~w is a great birthday!'-[Month, Day, Year]),
       p(String)])
  ;
  render_form(Month, MonthError, Day, DayError, Year, YearError, String)).
```

I need to update the above to use [http_redirect(+How, +To, +Request)](http://www.swi-prolog.org/pldoc/doc_for?object=http_redirect/3) which I only got to grips with in Unit 4.

#### Predicates with the same name and arity handling different cases

This style of *pattern-action* is another alien Prolog thing for those of us weaned on the C-family in that instead of dealing with different cases in one function, in Prolog each case tends to have its own predicate. In the first *form_handler(Request)* predicate, if the method is POST or data has been sent via GET because search(Anything) is in the Request list, it will skip rendering a blank form and move on to the second *form_handler(Request)* predicate which looks at the submitted data and then either asks for corrections or redirects to the success page.

I've put explicit tests at the top of both predicates to ensure the right predicate handles the right case. Checking patterns on the left side of the :- is generally safest, and if I wasn't making this example method agnostic, I'd rewrite the first case as ```form_handler(get, Request)``` and the second as ```form_handler(put, Request)``` and then change to ```:- http_handler('/', form_handler(Method), [method(Method), prefix]).``` to eliminate any ambiguity.

A potential problem in the way I've done it is that if one of the other [HTTP methods](https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods) is received &mdash; they include PUT, DELETE, HEAD, PATCH... &mdash; none of the above form_handler predicates will respond, leading to a confusing answer of *false*. Since whoever sends an HTTP method besides GET and POST is not using a browser, and is probably a hacker up to no good, responding with a server error message doesn't bother me too much here.

A common pitfall in this style of programming is more than one predicate may think it is the correct one for the given case &mdash; or worse yet, none accept the case, as would happen in this example with method(put), leading to the old joke "How many Prolog programmers does it take to change a lightbulb? *false*" &mdash; meaning it requires careful thought and testing.

An alternative way to have written the above would be to put an exclamation mark after all checks before the last predicate (! is called [cut](http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse44) in Prolog jargon) and then leave out any checks in the last predicate to make it the default, thereby avoiding the dreaded *false* while breaking the Zen of Python's tenth commandment "Errors should never pass silently".

You don't have to use a separate predicate for each case, and I relapsed to the C-style ```if -> then ; else``` pattern frowned on by Prolog purists (because it's often a sign of programmers too lazy to think through all cases carefully) in the second predicate to handle whether the form needs to be sent back with errors or to redirect to a success page.

The ```->``` is Prolog syntactic sugar for ```, !, ```... Exclamation marks trip me up, especically if there are several in a predicate, so I go for the sweeter version. If your predicate starts looking like ```if -> then ; else if -> then ;...``` you probably wan't to think about your patterns and actions more carefully by splitting them into separate predicates.

#### Using http_parameters built in tests

I've moved into a predicate called validate_form things which could be done more succinctly by options provided by http_parameters. If these tests fail, http_parameters throws a _400 Bad Request_ error, which could be caught with [catch(:Goal, +Catcher, :Recover)](http://www.swi-prolog.org/pldoc/doc_for?object=catch/3) using something like this:

```prolog
catch(http_parameters(Request,
    [month(Month, [oneof(['January','February','March','April','May','June','July','August','September','October','November','December'])]),
     day(Day, [between(1, 31)]),
     year(Year, [between(1890, 2030)])]),
  error(_, _),
  my_error_handler),
success_handler).  
```

A snag with the above is that if a value does not pass http_paramaters' test, it gets thrown away, leaving the variable allocated to it *unground*. This is why I took the more elaborate route of keeping bad input values to use for error messages and return to the browser for editing.

### Switching from GET to POST

A nice thing about http_parameters is that switching between GET and POST simply involves editing one line in server.pl:


```prolog
    [form([name('birthday'), action=('/'), method('GET')],
```
to
```prolog
    [form([name('birthday'), action=('/'), method('POST')],
```

Note that if you edit anything in the server.pl file, you need to kill the process and restart it before it takes effect.

### Validating in the browser

Though I haven't used it in the example since this is about writing server code, a better way to catch user typos and provide feedback is with Javascript in the browser, and I've included how I would go about that with [/scripts/validate_form.js](https://github.com/roblaing/swipl-webapp-howto/blob/master/unit2/scripts/validate_form.js).

Including it in the example would just involve adding ```onsubmit('return validateForm()')``` to the form attributes and linking to the Javascript file at the end of the body tag list.

```prolog
render_form(Month, MonthError, Day, DayError, Year, YearError, RequestString) :-
  reply_html_page(
    [title('Birthday'),
     link([rel('stylesheet'), href('/styles/basic.css')])],
    [form([name('birthday'), action=('/'), method('GET'), onsubmit('return validateForm()')],
      [h2('What is your birthday?'),
       div([label([for('month')], 'Month:'),
            input([type('text'), id('month'), name('month'), value(Month)]),
            span([class('error'), id('error_month')], MonthError)]),
       div([label([for('day')], 'Day:'),
            input([type('text'), id('day'), name('day'), value(Day)]),
            span([class('error'), id('error_day')], DayError)]),
       div([label([for('year')], 'Year:'),
            input([type('text'), id('year'), name('year'), value(Year)]),
            span([class('error'), id('error_year')], YearError)]),
       div([class="button"], button([type('submit')], 'Send your birthday'))]),
     p(RequestString),
     script([src('/scripts/validate_form.js')],'')]).
```

You would also need to add the Javascript file to the http_handler predicates (for some reason not having root as a static file seems to break the automatic loading of other files).

```prolog
:- http_handler('/',  form_handler, [prefix]).
:- http_handler('/styles/basic.css', http_reply_from_files('.', [indexes(['./styles/basic.css'])]), [prefix]).
:- http_handler('/scripts/validate_form.js', http_reply_from_files('.', [indexes(['./scripts/validate_form.js'])]), [prefix]).
```

While my Javascript code will prevent innocent typos getting transmitted, it won't catch malicious tampering of the input data which can easily be done by editing the URL when GET is used. So it's wise to follow a belt and braces approach of having both the client and server validate the user's input.


## Unit 3

> “Torture the data, and it will confess to anything.” ― Ronald Coase

This unit introduces an SQL database which SWI-Prolog communicates with via the [ODBC Interface](http://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/odbc.html%27)).

To avoid a slipup I ran into &mdash; trying this code on a new Postrgesql 11 server resulted in ```ERROR: ODBC: State 08001: [unixODBC]FATAL:  Ident authentication failed for user...``` &mdash; which caused me to malign ODBC and SWI Prolog in an earlier version of this document before I learnt you need to either install and start an ident daemon on your machine, or edit pg_hba.conf (found at 
/var/lib/pgsql/11/data/pg_hba.conf in Centos 7):

```
# IPv4 local connections:
host    all             all             127.0.0.1/32            ident
```

by changing *ident* to *trust*. Keeping the default ident and installing oidentd is the safer solution.

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

To insert the ASCII art into the database, I've used the following predicate:

```prolog
db_insert(Title, Art) :-
  odbc_connect('blog', Connection, []),
  odbc_prepare(Connection, 'INSERT INTO arts (title, art) VALUES (?, ?)', [default, default], Statement),
  odbc_execute(Statement, [Title, Art]),
  odbc_free_statement(Statement),
  odbc_disconnect(Connection).
```

Though overkill for this simple example, a good habit to get into with SWI Prolog is whenever you open and close a *stream* &mdash; be it a file, internet request, or database in this case &mdash; use [setup_call_cleanup(:Setup, :Goal, :Cleanup)](http://www.swi-prolog.org/pldoc/doc_for?object=setup_call_cleanup/3). This ensures good housekeeping if something goes wrong or the process gets interupted.

Whereas in this example there is only one SQL statement to prepare, I've found they tend to proliferate when one graduates from tutorials to proper web application development, and therefore it's handy to get into the good habit of using SWI Prolog's [Dicts](http://www.swi-prolog.org/pldoc/man?section=bidicts) to create a single container for the *Connection* and who knows how many prepared SQL Statements for various predicates to execute before finally freeing all these statements and closing the database.

SWI Prolog dictionaries are created using curly braces ```_{key1:value1, key2:value2, ...}``` and accessed using dots &mdash a notation Python and Javascript programmers will find familiar &mdash; or in a more prologish style of ```get_dict(?Key, +Dict, -Value)```.

My suggested way of doing things is along these lines:

```prolog
db_setup_call_cleanup(Title, Art) :-
  setup_call_cleanup(
    db_setup(Dict),
    % run your program here
    db_insert(Dict, Title, Art),
    db_cleanup(Dict).
  ).

db_setup(Dict) :-
  odbc_connect('blog', Connection, []),
  odbc_prepare(Connection, 'INSERT INTO arts (title, art) VALUES (?, ?)', [default, default], Statement),
  Dict = sql{connection:Connection, title_art: Statement}.
  
db_insert(Dict, Title, Art) :-
    odbc_execute(Dict.title_art, [Title, Art]).

db_cleanup(Dict) :-
  odbc_free_statement(Dict.title_art),
  odbc_disconnect(Dict.connection).  
```

Before learning the *correct* way to use SWI Prolog's ODBC predicates, I got tripped up by a trap the original course avoided by using Google's GQL database which had me cursing SWI Prolog until I realised it was actually SQL's fault: you can't use single quotes within SQL text unless they are escaped with another single quote. The test ASCII art I used, obtained from <https://www.asciiart.eu/>, was this:

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

This little Linux penguin caused server.pl to keep barfing ```odbc: state 42601: error: unterminated quoted string at or near...``` until I figured out you need to check the input text for single quotes, and if so escape them with another single quote.

A better way do it was using odbc_prepare and odbc_execute as in this [example](https://github.com/SWI-Prolog/packages-odbc/blob/master/demo/wordnet.pl).

Before discovering odbc_prepare, I solved this problem like so with a helper predicate to replace single quotes with double quotes:

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

Even though I ended up deleting sql_escape_single_quotes, it's still a handy template for predicates needed for replacing stuff in strings which will come in handy later. 

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

The way I've written the code in this unit dates back to before I decided to do HTML generating the prologish way, so I'll return to update it later.

## Unit 4

> Everything is easy when you don't know what you're talking about. &mdash; Quote I heard somewhere but can't remember where, which came to be very relevant writing this unit.

In this unit we step into the dangerous minefield of using cookies to authenticate users. Users will be stored in our blog database in the following table:

```sql
CREATE TABLE IF NOT EXISTS users (
    id     TEXT PRIMARY KEY,
    name   TEXT UNIQUE NOT NULL,
    email  TEXT
);
```

Note there is no field for password in this table because passwords should never be seen let alone stored in plain text by web servers &mdash; an elementary part of online security which a shocking number of big corporations fail to grasp.

This is related to the id type being a string despite the general database rule of thumb that integers make more efficient keys.

Creating the id introduces us to SWI Prolog's [SHA* Secure Hash Algorithms](http://www.swi-prolog.org/pldoc/man?section=sha) library. The hashing function we're using, sha256, produces a (with extremely rare exceptions) unique 256 character string which we make different from the user_id read as a cookie from the user's browser by mixing it with some secret salt and rehashing it like so:

```prolog
:- use_module(library(sha)).

create_hash(String, HexDigest) :-
  Salt = "Some very long randomly generated string",
  hmac_sha(Salt, String, HMAC, [algorithm(sha256)]),
  hash_atom(HMAC, HexDigest).
```

The string rehashed above was created by the browser from the user's login and password with Javascript's [Subtle​Crypto​.digest()](https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto/digest) mixed with some different secret salt stored as a hidden field in the login and registration forms.

Thereby we create an irreversible (but reproducible) list of unique gibberish to store as an id cookie on the user's browser for the server to read. 

If we point the browser at http://localhost:3030/signup and select *John Smith* as the username and *Password1* as the password, the server sees the following cookie appear in the Request list:

```
cookie([user_id=aca87862328161c8d5cc6b95d29c04401df3d4496001ba54748fed7719834a0c])
```

The same combination of login, password, secret salt, and hash function in any browser will reproduce the same cookie, making it inexcusable to send passwords over the internet to servers.

There's not much we can do about the owners of logins and passwords seeing the hash strings generated from them, but hopefully they don't give bad guys free access to their computers while logged into our website. Preventing someone with a packet sniffer seeing this cookie en route from the browser to the server would involve switching from http to https as described for SWI Prolog in this [tutorial](https://github.com/triska/letswicrypt). 

I recently did this for a SWI Prolog powered site behind nginx, following the steps at [Digital Ocean](http://www.digitalocean.com/?refcode=a32a25b52821) and found it pleasantly easy. Nothing needs to be done in the server.pl script, assuming it's using nginx or Apache as a "receptionist".

### Cookies in SWI Prolog

SWI Prolog has an [HTTP Session management](http://www.swi-prolog.org/pldoc/man?section=httpsession) library which sets a cookie swipl_session='Some unique ID' which is used by predicates akin to Prolog's standard clausal store manipulators assert and retract for the server to remember temporary things about a specific user. But that's not really the right tool for the job here.

Luckily, checking if there are cookies in the Request, and if so reading the value of a specific key if it exists simply requires two clauses at the top of the *logged_in* predicate which will return *false* if the user is not logged in or the login and password combination on the browser isn't valid:

```prolog
logged_in(Request, User) :-
  member(cookie(Cookies), Request),
  member(user_id=UserId, Cookies),
  create_hash(UserId, Id),
  odbc_connect('blog', Connection, []),
  odbc_query(Connection, "SELECT name FROM users WHERE id = '~w'"-[Id], row(User)),  
  odbc_disconnect(Connection).
```
To safeguard things server-side, we can't simply use the browser cookie as our id in the database, but need to rehash it as suggested above. For our John Smith with Password1 example, the previous hash gets turned into a completely different unique id of 
```21b07bc6c590b4b826d8786b837c859e740d9d1a1e9cbfdfcc3c05c299f5f62d``` 
for the database without the password ever leaving the browser to be accessible by bad guys en route. And whoever can read stuff in our database legitimately or illegitimately can't use the id to hijack the user's account.

The above predicate will return true with the User's name if a browser cookie has been set by a valid login and password combination, or false in which case the web application can redirect to /login like so:

```prolog
welcome_or_login(Request) :-
  ( logged_in(Request, Name) -> 
    term_string(Request, String),
    render_welcome(Name, String)
  ; 
    http_redirect(see_other, root(login), Request)).
```

I originally wrote the above to call the login_handler instead of redirecting, but then found the home page kept the /login path, and testing password errors (when the login page keeps you there) got buggy... so found http_redirect a big help.

### Web page logic

This little exercise involves guiding users to the correct page. Initially the home page will redirect to /login if the user is not logged in, and the login page needs a link to a /signup page for new users who haven't yet registered. The home page has a link to /logout which deletes the cookie, done entirely as a [static file](https://github.com/roblaing/swipl-webapp-howto/blob/master/unit4/logout.html), and then redirects to /login.

For the sake of learning, I've prevented users from logging on as someone else before logging out, or registering for new accounts while logged into an existing account. That's probably not really necessary, but it did affirm my view that a logical programming language is a good choice for web development because making these rules came very naturally.

## Unit 5

> “If you want to master something, teach it.” ― Richard Feynman

```prolog
setup_call_cleanup(
        http_open(URL, In, []),
        process(In),
        close(In)).
```


Work in progress...

Here we go into SWI Prolog's [HTTP client libraries](http://www.swi-prolog.org/pldoc/man?section=http-clients) to get data programmatically from other servers to use in our code.

[http_open(+URL, -Stream, +Options)](http://www.swi-prolog.org/pldoc/man?predicate=http_open/3)

Options




```prolog
consult(library(http/http_open)).

  http_open('http://www.google.com/search?q=prolog', In, []),
  copy_stream_data(In, user_output),
  close(In).
```

https://openweathermap.org/guide

:- use_module(library(uri)).

[uri_encoded(+Component, +Value, -Encoded)](http://www.swi-prolog.org/pldoc/doc_for?object=uri_encoded/3)

uri_components(HREF0, Components),
uri_data(search, Components, Query),
uri_query_components(Query, Parts),
        
