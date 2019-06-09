:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_unix_daemon)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(http/json)).

:- initialization http_daemon.

:- http_handler(root(.), weatherapp, []).
:- http_handler('/images/01d.png', http_reply_from_files('.', [indexes(['./images/01d.png'])]), [prefix]).
:- http_handler('/images/02d.png', http_reply_from_files('.', [indexes(['./images/02d.png'])]), [prefix]).
% Need to figure out how to load all images using dynamic files

weatherapp(_Request) :-
  URL = 'https://samples.openweathermap.org/data/2.5/forecast/daily?id=524901&appid=b1b15e88fa797225412429c1c50c122a1',
  get_data(URL, Dict),
  length(Dict.list, Length),
  weather_rows(1, Length, Dict.list, Rows),
  reply_html_page(
     [title("~w's Weather Page"-[Dict.city.name])],
     [table([], [th([colspan('3')],["~w's weather over the next ~w days"-[Dict.city.name, Dict.cnt]])|Rows])]).

get_data(URL, Dict) :-
  setup_call_cleanup(
    http_open(URL, In, []),
    json_read_dict(In, Dict),
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

