# Unit 4 User authentication

*By Robert “Joe Blog” Laing*

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

