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

Each unit's subdirectory has a SWI Prolog file called *server.pl* along with a README.md file for that tutorial. Until recently, everything was on this page, but I decided to "chapterize" as my tutorial started turning into a book. 

I've addopted a common subdirectory structure in each unit where html files are in the root directory with server.pl (though there's nothing stopping you from putting them in their own subdirectory), jpg, png etc go in images, css files in styles and javascript in scripts.

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

## Unit 1 [Handlers and generating HTML](https://github.com/roblaing/swipl-webapp-howto/tree/master/unit1)

This unit introduces SWI Prologs' [http_handler(+Path, :Closure, +Options)](http://www.swi-prolog.org/pldoc/doc_for?object=http_handler/3) and its [reply_html_page(:Head, :Body)](http://www.swi-prolog.org/pldoc/doc_for?object=reply_html_page/2) predicates, which are fundamental to using it as a web application development language.

I've also explained the basics of reading Prolog documentation &mdash; something I suspect most novices will battle with.

## Unit 2 [HTML Forms with GET and POST](https://github.com/roblaing/swipl-webapp-howto/tree/master/unit2)

This unit covers the basics of web forms, which SWI Prolog makes fairly easy with one predicate [http_parameters(+Request, ?Parameters)](http://www.swi-prolog.org/pldoc/doc_for?object=http_parameters/2) which can be used without modification for GET or POST, and allows you to validate or give default values to the incoming key=value list.

## Unit 3 [Linking to a database](https://github.com/roblaing/swipl-webapp-howto/tree/master/unit3)

This unit covers hooking SWI Prolog to an SQL database. I like Postgresql, but thanks to the [ODBC Interface](http://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/odbc.html%27)), you can pick just about any database you like.

This also introduces SWI Prolog's [Dicts](http://www.swi-prolog.org/pldoc/man?section=bidicts) which are a handy way to circumvent the prolog problem of having to pass too many arguments between predicates.

## Unit 4 [User authentication](https://github.com/roblaing/swipl-webapp-howto/tree/master/unit4)

In this unit we create a system whereby a user identifies themself to the server without a password ever leaving the browser. Part of this involves SWI Prolog's [SHA* Secure Hash Algorithms](http://www.swi-prolog.org/pldoc/man?section=sha) library.

## Unit 5 [Web services](https://github.com/roblaing/swipl-webapp-howto/tree/master/unit5)

This unit looks at getting data from an external source &mdash; I've used <https://openweathermap.org/> for this example &mdash; using [http_open(+URL, -Stream, +Options)](http://www.swi-prolog.org/pldoc/doc_for?object=http_open/3) and introduces SWI Prolog's support for Json and XML.


