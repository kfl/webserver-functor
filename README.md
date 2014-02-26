App Webserver As a Functor
==========================

Example code demonstrating how SML modules, in particular functors,
can be used for structuring a web framework.

The code in its current state is only useful for demonstration, it is
**not** ready for production. Also, it's probably a bit far fetched to
call the current code a "framework".


Architecture
------------

The overall idea is that you make a web-application by implementing a
structure, say `App`, with the signature `WebApp`. Then you give `App`
to a functor which will then take care of creating a web server (or in
communicate with one).

The framework consists of two files:

* `HtmlUtils.sml` contains a structure `HtmlUtils` where the type
  `reply` for HTTP replies is declared, the module also contains some
  simple-mined (read inefficient) utility functions for creating HTML
  pages.

* `WebServerFct.sml` contains the signature `WebApp` and the functor
  `WebServerFct`.


Examples
--------

Compile the examples with the command:

~~~
$ mosmlc -toplevel HtmlUtils.sml WebServerFct.sml App.sml -o appserver
~~~

Where `App.sml` is the example you want to compile.

* `CountingEcho.sml` an echo server, that also counts how many
  times it has been called.

* `PhonebookApp.sml` an phonebook with shared state across clients.
   Uses an in-memory "database" implemented as a list.

   Shows how to deal (rudimentary) with the query
   path of an URL (<http://en.wikipedia.org/wiki/Query_string>).
