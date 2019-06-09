# Unit 5 Web services

Work in progress...

This unit looks at getting data from an external source &mdash; I've used <https://openweathermap.org/> for this example &mdash; using [http_open(+URL, -Stream, +Options)](http://www.swi-prolog.org/pldoc/doc_for?object=http_open/3).

As is common with web-based service providers, OpenWeatherMap offers its data in either xml or Json format. Though [SWI Prolog supports xml parsing](http://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/sgml.html%27)), mercifully Json tends to be the default these days and SWI Prolog has combined this with its [dicts](http://www.swi-prolog.org/pldoc/man?section=bidicts), making Json the much easier choice. 

In the example code, I've used the sample URL from OpenWeatherMap's [guide](https://openweathermap.org/guide), offering Moscow's weather forecast for the coming week. Getting your own access key and editing the URL to give a 16 day weather forecast for wherever you live is a free and simple process. Please respect OpenWeatherMap's request not to hit its servers more than once every ten minutes (so don't put this demo live on the web with a real user account).

As with database queries, we want to put the three step process of opening a stream, processing the received data, then closing the stream within [setup_call_cleanup(:Setup, :Goal, :Cleanup)](http://www.swi-prolog.org/pldoc/doc_for?object=setup_call_cleanup/3).

```prolog
get_data(URL, Dict) :-
  setup_call_cleanup(
    http_open(URL, In, []),
    json_read_dict(In, Dict),
    close(In)
  ).
```

[json_read_dict(+Stream, -Dict)](http://www.swi-prolog.org/pldoc/doc_for?object=json_read_dict/2), which requires us to add ```:- use_module(library(http/json)).``` to the list at the top of server.pl, making the Json-formatted weather data accessible via the familiar (to Javascript and Python programmers) dot notation.

Translating the supplied data into an HTML table involves eyeballing the supplied Json text, staring directly into the [agglutinative](https://en.wikipedia.org/wiki/Agglutination) horror of object-orientation which leads to names like *Dict.city.name*.

The supplied data includes a list, handily called list, of elements which look like this:

```json
_40532{clouds:0,
       deg:225,
       dt:1485766800,
       humidity:76,
       pressure:1024.53,
       snow:0.01,
       speed:4.57,
       temp:_40346{day:262.65,eve:262.65,max:262.65,min:261.41,morn:262.65,night:261.41},
       weather:[_40464{description:"sky is clear",icon:"01d",id:800,main:"Clear"}]}
```

OpenWeatherMap does not include dates, but the list goes from today up to 16 days ahead (if you have applied for your own app key, and included ```...&cnt=16&...``` in the url.

Including a date in the weather forecast table forced me to refresh my knowledge of time stamps and date formatting which I avoided in Unit 2.


