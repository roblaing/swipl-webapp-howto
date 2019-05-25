:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_unix_daemon)).
:- use_module(library(http/http_parameters)).

:- initialization http_daemon.

:- http_handler('/',  form_handler, [prefix]).
:- http_handler('/styles/basic.css', http_reply_from_files('.', [indexes(['./styles/basic.css'])]), [prefix]).

% Fresh form with all blank fields
form_handler(Request) :-
  term_string(Request, String),
  memberchk(method(get), Request),
  \+memberchk(search(_), Request), !,
  render_form('', '', '', '', '', '', String).  

% Submitted form that needs validity test
form_handler(Request) :-
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

render_form(Month, MonthError, Day, DayError, Year, YearError, RequestString) :-
  reply_html_page(
    [title('Birthday'),
     link([rel('stylesheet'), href('/styles/basic.css')])],
    [form([name('birthday'), action=('/'), method('GET')],
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
     p(RequestString)]).

validate_form(Month, DayStr, YearStr, MonthError, DayError, YearError) :-
  (Month = '' -> MonthError = 'Missing month' ; 
   (memberchk(Month, ['January','February','March','April','May','June','July','August','September','October','November','December']) ->
   MonthError = '' ; string_concat(Month, ' is not a valid month', MonthError))),
  (DayStr = '' -> DayError = 'Missing day' ;
   (term_to_atom(Day, DayStr), between(1, 31, Day) ->
    DayError = '' ; string_concat(DayStr, ' is not a valid day of the month', DayError))),
  (YearStr = '' -> YearError = 'Missing year' ;
   (term_to_atom(Year, YearStr), between(1890, 2030, Year) ->
    YearError = '' ; string_concat(YearStr, ' is not a likely birth year', YearError))).

