
check_name(Username) :-
  odbc_connect('blog', Connection, []),
  odbc_query(Connection, "SELECT name FROM users WHERE name = '~w'"-[Username], row(_)),  
  odbc_disconnect(Connection).

