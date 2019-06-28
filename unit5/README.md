# Unit 5 Web services

*By Robert “Joe Blog” Laing*

> Don't worry about people stealing your ideas. If your ideas are any good, you'll have to ram them down people's throats. &mdash; Howard H. Aiken

This unit looks at getting data from an external source &mdash; I've used <https://openweathermap.org/> for this example &mdash; using [http_open(+URL, -Stream, +Options)](http://www.swi-prolog.org/pldoc/doc_for?object=http_open/3).

## Handling timeouts with SWI Prolog

When fetching data from other servers, we need to make a contingency plan for when the service provider is offline. Here I'm using SWI Prolog's predicate [call_with_time_limit(+Time, :Goal)](https://www.swi-prolog.org/pldoc/doc_for?object=call_with_time_limit/2) and an answer to a [StackOverflow](https://stackoverflow.com/questions/23900469/catch-3-and-call-with-time-limit-2-predicates-in-swi-prolog) question given by [Paulo Moura](https://logtalk.org/).

```prolog
(  catch(call_with_time_limit(Time, Goal), Error, true) 
-> (  var(Error) 
   -> % success actions
   ;  % error actions
   )
;  % failure actions
).
```

Incidently, one of the options to [http_handler(+Path, :Closure, +Options)](https://www.swi-prolog.org/pldoc/doc_for?object=http_handler/3) is time_limit(+Spec) which defaults to 300 seconds, or 5 minutes, before throwing an error.


## Json

As is common with web-based service providers, OpenWeatherMap offers its data in either xml or Json format. Mercifully Json tends to be the default these days and SWI Prolog has combined this with its [dicts](http://www.swi-prolog.org/pldoc/man?section=bidicts), making Json the much easier choice. 

In the example code, I've used the sample URL from OpenWeatherMap's [guide](https://openweathermap.org/guide), offering Moscow's weather forecast for the coming week. Getting your own access key and editing the URL to give a 16 day weather forecast for wherever you live is a free and simple process. Please respect OpenWeatherMap's request not to hit its servers more than once every ten minutes (so don't put this demo live on the web with a real user account).

As with database queries, we want to put the three step process of opening a stream, processing the received data, then closing the stream within [setup_call_cleanup(:Setup, :Goal, :Cleanup)](http://www.swi-prolog.org/pldoc/doc_for?object=setup_call_cleanup/3).

```prolog
get_json(URL, Dict) :-
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

## XML

I've included an example of how to use SWI Prolog's [SGML/XML parser](https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/sgml.html%27)), but OpenWeatherMap doesn't offer a sample URL identical the above, so this shows today's weather in London instead.

Extracting data from XML documents involves learning yet another query language, [XPath](https://en.wikipedia.org/wiki/XPath) which uses a notation akin to the Unix file system, which SWI Prolog supports via [library(xpath)](https://www.swi-prolog.org/pldoc/man?section=xpath).

The sample XML document returned from OpenWeatherMap looks like this:

```xml
<current>
  <city id="2643743" name="London">
    <coord lon="-0.13" lat="51.51"/>
    <country>GB</country>
    <sun rise="2017-01-30T07:40:36" set="2017-01-30T16:47:56"/>
  </city>
  <temperature value="280.15" min="278.15" max="281.15" unit="kelvin"/>
  <humidity value="81" unit="%"/>
  <pressure value="1012" unit="hPa"/>
  <wind>
    <speed value="4.6" name="Gentle Breeze"/>
    <gusts/>
    <direction value="90" code="E" name="East"/>
  </wind>
  <clouds value="90" name="overcast clouds"/>
  <visibility value="10000"/>
  <precipitation mode="no"/>
  <weather number="701" value="mist" icon="50d"/>
  <lastupdate value="2017-01-30T15:50:00"/>
</current>
```

Getting the xml data is nearly identical to getting the Json data, just substituting json_read_dict with load_xml which translates the XML document above into a Prologish list. 

```prolog
get_xml(URL, Xml) :-
  setup_call_cleanup(
    http_open(URL, In, []),
    load_xml(In, Xml, []),
    close(In)
  ).
```

Using [xpath(+DOM, +Spec, ?Content)](https://www.swi-prolog.org/pldoc/doc_for?object=xpath/3), I managed to extract "London" with ```xpath(Xml, //city(@name), Name)```.



