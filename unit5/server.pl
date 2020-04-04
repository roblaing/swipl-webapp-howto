:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_unix_daemon)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(http/json)).
:- use_module(library(sgml)).
:- use_module(library(xpath)).

:- initialization(http_daemon, main).

:- multifile http:location/3.
:- dynamic   http:location/3.
http:location(images, root(images), []).

:- http_handler(root(.), weatherapp_json, []).
:- http_handler(root(xml), weatherapp_xml, []).
:- http_handler(images(.), http_reply_from_files('./images', []), [prefix]).

weatherapp_json(_Request) :-
  URL = 'https://samples.openweathermap.org/data/2.5/forecast/daily?id=524901&appid=b1b15e88fa797225412429c1c50c122a1',
  (  catch(call_with_time_limit(20, get_json(URL, Dict)), Error, true) 
  -> (  var(Error) 
     -> length(Dict.list, Length),
        weather_rows(1, Length, Dict.list, Rows),
        reply_html_page(
          [title("~w's Weather Page"-[Dict.city.name])],
          [table([], [th([colspan('3')],["~w's weather over the next ~w days"-[Dict.city.name, Dict.cnt]])|Rows])])
     ;  reply_html_page(
          [title('Error Page')],
          [p('Oops! '-[Error])])
     )
  ; reply_html_page(
          [title('Failure Page')],
          [p('Oops! Something failed.')])
  ).

get_json(URL, Dict) :-
  setup_call_cleanup(
    http_open(URL, In, []),
    json_read_dict(In, Dict, [default_tag(json)]),
    close(In)
  ).

getday(Idx, DayOfWeek) :-
  get_time(TimeStamp), 
  ComingTimeStamp is TimeStamp + ((Idx - 1) * 86400),
  stamp_date_time(ComingTimeStamp, date(Year, Month, Day, Hour, Minute, Second, UTCOffset, TimeZone, Y), 'UTC'),
  format_time(atom(DayOfWeek), '%A, %e %B %Y',
     date(Year, Month, Day, Hour, Minute, Second, UTCOffset, TimeZone, Y), posix).

html_weather_row(Idx, Day, Tr) :-
  Day.weather = [Weather],
  getday(Idx, DayOfWeek),
  Tr = tr([],[td([],[DayOfWeek]),
              td([],['Day: ~w, Night: ~w, Max: ~w, Min: ~w'-[Day.temp.day, Day.temp.night, Day.temp.max, Day.temp.min]]),
              td([],[img([src('/images/~w.png'-[Weather.icon]), alt(Weather.description)],[])])]). 
  
weather_rows(Length, Length, [Day], [Row]) :-
  html_weather_row(Length, Day, Row), !.

weather_rows(Idx, Length, [Day|Days], [Row|Rows]) :-
  html_weather_row(Idx, Day, Row),
  IdxInc is Idx + 1,
  weather_rows(IdxInc, Length, Days, Rows).
  
weatherapp_xml(_Request) :-
  URL = 'https://samples.openweathermap.org/data/2.5/weather?q=London&mode=xml&appid=b6907d289e10d714a6e88b30761fae22',
  (  catch(call_with_time_limit(20, get_xml(URL, Xml)), Error, true) 
  -> (  var(Error) 
     -> get_xml(URL, Xml),
        term_string(Xml, Str),
        xpath(Xml, //city(@name), Name),
        reply_html_page(
          [title("~w's Current Weather Page"-[Name])],
          [p(Str)])
     ;  reply_html_page(
          [title('Error Page')],
          [p('Oops! '-[Error])])
     )
  ; reply_html_page(
          [title('Failure Page')],
          [p('Oops! Something failed.')])
  ).
 
get_xml(URL, Xml) :-
  setup_call_cleanup(
    http_open(URL, In, []),
    load_xml(In, Xml, []),
    close(In)
  ).


