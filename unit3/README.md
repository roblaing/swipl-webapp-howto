# Unit 3 Linking to a database

*By Robert “Joe Blog” Laing*

> “Torture the data, and it will confess to anything.” ― Ronald Coase

This unit introduces an SQL database which SWI-Prolog communicates with via the [ODBC Interface](http://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/odbc.html%27)).

ODBC was designed by Microsoft, so will get us Linux users cursing, but once setup seems to work pretty well.


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

Trying the code below on a new Postrgesql 11 server resulted in ```ERROR: ODBC: State 08001: [unixODBC]FATAL:  Ident authentication failed for user...``` &mdash; which caused me to malign ODBC and SWI Prolog in an earlier version of this document before I learnt you need to either install and start an ident daemon on your machine, or edit pg_hba.conf (found at 
/var/lib/pgsql/11/data/pg_hba.conf in Centos 7):

```
# IPv4 local connections:
host    all             all             127.0.0.1/32            ident
```

by changing *ident* to *trust*. Keeping the default ident and installing oidentd is the safer solution.

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

A good habit to get into with SWI Prolog is whenever you open and close a *stream* &mdash; be it a file, internet request, or database in this case &mdash; use [setup_call_cleanup(:Setup, :Goal, :Cleanup)](http://www.swi-prolog.org/pldoc/doc_for?object=setup_call_cleanup/3). This ensures good housekeeping if something goes wrong or the process gets interrupted.

The way I obtain the list of ASCII art from the database makes a nice, simple illustration of this technique:

```prolog
db_select(ArtList) :-
  setup_call_cleanup(
    odbc_connect('blog', Connection, []),
    odbc_query(Connection, "SELECT title, art FROM arts ORDER BY created DESC", 
      ArtList, [findall(row(Title, Art), row(Title, Art))]),  
    odbc_disconnect(Connection)
  ).
```

Inserting stuff into the database initially led me into a trap &mdash; not escaping single quotes within strings is the cause of a notorious problem known as SQL injection attacks &mdash; so I'll start with how I ended up doing it and then digress into my initial, bad attempts.

## SWI Prolog dictionaries

This is also a good place to introduce an expansion SWI Prolog has made to traditional prolog by introducing [dictionaries](http://www.swi-prolog.org/pldoc/man?section=bidicts) created using curly braces ```_{key1:value1, key2:value2, ...}``` and accessed using dots &mdash; a notation Python and Javascript programmers will find familiar.

A [tutorial on using dicts](https://swish.swi-prolog.org/example/dict.swinb) is available on Swish.

For those who don't like to pollute their Prolog code with foreign syntax, prologish alternatives such as ```get_dict(?Key, +Dict, -Value)``` are available. 

I find dictionaries a great addition to Prolog in that they circumvent the problem in traditional Prolog of a proliferation of *context arguments* to pass on to predicates since the original designers were opposed to lexical or any other kind of scoping. Creating an *environment collection* could also be done by storing everything in a list ```[key1(value1), key2(value2), ...]``` and then retrieving a specific key-value with ```member(key(value), List)``` as in previous units to get things from the *Request* list, but since I hop between Prolog and Javascript a lot, I like the curly bracket and dot notation. Understanding SWI Prolog's dicts are also important for importing and exporting data as Json in Unit 5.

Whereas in this example there is only one SQL statement to prepare, I've found they tend to proliferate when one graduates from tutorials to proper web application development, and therefore it's handy to be able to bung all these variables into one container.

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
  Dict = sql{connection: Connection, title_art: Statement}.
  
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

A lot of web application development boils down to list processing, which is again one of Prolog's strong suits &mdash; though not easily used because it's very different to conventional programming languages. I've written a tutorial at <https://swish.swi-prolog.org/p/Iteration2.swinb> to get my own head around the basics of iterating through lists in Prolog, touching on its Definite Clause Grammar (DCG) notation used by SWI Prolog's html_write library. I'm far from a master at this stuff, but do know enough to get an idea of how powerful it can be.

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


