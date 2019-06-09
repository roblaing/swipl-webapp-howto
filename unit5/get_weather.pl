:- use_module(library(http/http_open)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(http/json)).


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
  
/*
_17498{city:_15174{country:"RU",geoname_id:524901,iso2:"RU",lat:55.7522,lon:37.6156,name:"Moscow",population:0,type:"city"},
       cnt:7,
       cod:"200",

list
_40532{clouds:0,
       deg:225,
       dt:1485766800,
       humidity:76,
       pressure:1024.53,
       snow:0.01,
       speed:4.57,
       temp:_40346{day:262.65,eve:262.65,max:262.65,min:261.41,morn:262.65,night:261.41},
       weather:[_40464{description:"sky is clear",icon:"01d",id:800,main:"Clear"}]}
*/

test :-
  URL = 'https://samples.openweathermap.org/data/2.5/forecast/daily?id=524901&appid=b1b15e88fa797225412429c1c50c122a1',
  get_data(URL, Data),
  render_table(Data, Table),
  print(Table).

render_table(Dict, Table) :-
  length(Dict.list, Length),
  weather_rows(1, Length, Dict.list, Rows),
  Table = [table([], [th([colspan('3')],["~w's weather over the next ~w days"-[Dict.city.name, Dict.cnt]]), Rows])].

get_data(URL, Dict) :-
  setup_call_cleanup(
    http_open(URL, In, []),
    json_read_dict(In, Dict),
    close(In)
  ).
  

