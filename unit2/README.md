# Unit 2 HTML Forms with GET and POST

*By Robert “Joe Blog” Laing*

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

