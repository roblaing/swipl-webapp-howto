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

Each unit's subdirectory has a SWI Prolog file called *server.pl* along with a README.md file for that tutorial. Until recently, everything was on this page, but I decided to "chapterize" as my tutorial started turning into a book. I've addopted a common subdirectory structure where html files are in the root directory with server.pl (though there's nothing stopping you from putting them in their own subdirectory), jpg, png etc go in images, css files in styles and javascript in scripts.

```
unit1
├── server.pl
├── index.html
├── about.html
├── images
│   └── swipl.png
├── scripts
│   └── form_validator.js
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

As this tutorial developed, it took a couple of digressions into providing tips to Prolog novices, which experienced Prolog programmers can simply skip.

## Unit 1 Handlers and generating HTML

This unit introduces SWI Prologs' [http_handler(+Path, :Closure, +Options)](http://www.swi-prolog.org/pldoc/doc_for?object=http_handler/3) and its [reply_html_page(:Head, :Body)](http://www.swi-prolog.org/pldoc/doc_for?object=reply_html_page/2) predicates, which are fundamental to using it as a web application development language.

I've also explained the basics of reading Prolog documentation &mdash; something I suspect most novices will battle with.

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


## Unit 3 [Linking to a database](https://github.com/roblaing/swipl-webapp-howto/tree/master/unit3)

This unit covers hooking SWI Prolog to an SQL database. I like Postgresql, but thanks to the [ODBC Interface](http://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/odbc.html%27)), you can pick just about any database you like.

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
        
