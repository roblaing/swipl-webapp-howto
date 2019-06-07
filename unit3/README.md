## Unit 3 Linking to a database

*By Robert “Joe Blog” Laing*

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

Though overkill for this simple example, a good habit to get into with SWI Prolog is whenever you open and close a *stream* &mdash; be it a file, internet request, or database in this case &mdash; use [setup_call_cleanup(:Setup, :Goal, :Cleanup)](http://www.swi-prolog.org/pldoc/doc_for?object=setup_call_cleanup/3). This ensures good housekeeping if something goes wrong or the process gets interrupted.

Whereas in this example there is only one SQL statement to prepare, I've found they tend to proliferate when one graduates from tutorials to proper web application development, and therefore it's handy to get into the good habit of using SWI Prolog's [Dicts](http://www.swi-prolog.org/pldoc/man?section=bidicts) to create a single container for the *Connection* and who knows how many prepared SQL statements for various predicates to execute before finally freeing all these statements and closing the database.

SWI Prolog dictionaries are created using curly braces ```_{key1:value1, key2:value2, ...}``` and accessed using dots &mdash; a notation Python and Javascript programmers will find familiar &mdash; or in a more prologish style of ```get_dict(?Key, +Dict, -Value)```.

My suggested way of doing things is along these lines:

```prolog
db_setup_call_cleanup(Title, Art) :-
  setup_call_cleanup(
    db_setup(Dict),
    % run your program here
    db_insert(Dict, Title, Art),
    db_cleanup(Dict)
  ).

db_setup(Dict) :-
  odbc_connect('blog', Connection, []),
  odbc_prepare(Connection, 'INSERT INTO arts (title, art) VALUES (?, ?)', [default, default], Statement),
  % add more odbc_prepare(...) predicates, remembering to also add them to the dictionary.
  Dict = sql{connection:Connection, title_art: Statement}.
  
db_insert(Dict, Title, Art) :-
    odbc_execute(Dict.title_art, [Title, Art]).

db_cleanup(Dict) :-
  odbc_free_statement(Dict.title_art),
  % remember to free whatever other SQL statements you add to db_setup here.
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
