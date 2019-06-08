# Unit 1 Handlers and generating HTML

*By Robert “Joe Blog” Laing*

> “When one teaches, two learn.” ― Robert Heinlein

Unit 1 in the original course was devoted to explaining the basics of HTML, which I assume readers are familiar with &mdash; if not I recommend Mozilla's [Getting started with the Web](https://developer.mozilla.org/en-US/docs/Learn/Getting_started_with_the_web) &mdash; so I'll kick-off with basics of a SWI Prolog webserver.

### Handlers

At the heart of the various web frameworks I've encountered over the years (Zope, Ruby on Rails, Django...) is parsing the directory structure
of incoming URLs into commands for the server to respond to. Pattern matching is one of Prolog's strongest suits, making slicing up URLs and dispatching
requests to the relevant handler shorter and sweeter than any of the _regex_ languages I've wrestled with.

The key predicate here is [http_handler(+Path, :Closure, +Options)](http://www.swi-prolog.org/pldoc/doc_for?object=http_handler/3), and I've included
a couple of ways to use it in the introductory example.

#### Howto read Prolog documentation

Experienced Prolog programmers tend to find it obvious that the [+, -, or ? prefixes](http://www.swi-prolog.org/pldoc/man?section=modes) to arguments in the documentation tells you if you are dealing with an input, an output, or a *bidirectional* argument. 

There are no output arguments (- prefixes) in http_handler, making it effectively a procedure rather than a function, but it does have a colon before the Closure, which is worth a digression since even intermediate Prolog programmers are likely to find it confusing.

A frustration I've had with SWI Prolog documentation is the lack of examples, and something I've discovered while writing this tutorial is to turn to <https://github.com/SWI-Prolog/packages-http/tree/master/examples> when I get lost.

One of the things that tripped me up learning Prolog was that you need to think in terms of input and output arguments within relations &mdash; to borrow spreadsheet or database jargon, think of relations as rows and arguments as columns with known data (inputs) or as columns which need to be calculated (outputs)  &mdash; which is somewhat alien to most of us brought up on programming languages with functions which substitute themselves into a return value.

If your query is so specific it only returns one row, it is *det* in Prolog jargon. If your result produces several rows, it is [nondet](http://www.swi-prolog.org/pldoc/man?section=unitbox), and you'll either want to iterate through these multiple answers, or figure out the bug in your *pattern-action* predicates covered in Unit 2. Much of web development boils down to list processing, so iteration will come up a lot in this tutorial. Getting to grips with the many ways of iterating tends to be quite a hurdle for anyone learning Prolog, which I've covered in a separate [tutorial](https://swish.swi-prolog.org/p/yeQhnQSk.swinb).

##### What is :Closure?

Long story short, [closures](https://simple.wikipedia.org/wiki/Closure_(computer_science)) are predicates some of whose arguments magically vanish when they are used as arguments in other predicates, but through some conjuring trick, reappear where the given predicate is declared. 

Whatever predicate you put as http_handler's second (ie :Closure) argument in turn has a hidden final argument &mdash; conventionally called *Request* &mdash; which gets passed to your handler as its final argument. In the static files examples in the next section which uses a library predicate, [http_reply_from_files(+Dir, +Options, +Request)](http://www.swi-prolog.org/pldoc/doc_for?object=http_reply_from_files/3), we can ignore *Request*, but it plays a leading role in subsequent examples when we write our own handlers.

It turns out *Request* is a list containing the HTTP key-value pairs (translated into Prolog functor(argument(s)) clauses), sent from the browser, looking something like ```[protocol(http),method(get),request_uri('/user/Joe%20Blog'),http_version(1-1),...]``` from which we can extract the value of a desired HTTP key with, say, ```member(request_uri(URI), Request)```. Using lists or dictionaries (introduced in Unit 3) as containers for *context arguments* is a handy way to make up for Prolog's traditional anti-scoping ideology.

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
