Braided
=======

Braided attempts to provide functionality similar to Deferred.js
It provides a mechanism for callbacks, errorbacks, completion, and fail states.

Why should I use this?
----------------------

Managing asynchronous behaviour in Erlang can occasionally be tricky. Ever wanted to perform a set of
asynchronous operations, but be notified of failures, successes, and when it is all done?

The braid object model allows you to setup callbacks, errbacks, generators, handlers, and actions.
This simple yet flexible set of functionality helps keep your code clean and concise.

Requirements
------------

Erlang, as the very least.

Rebar is the preferred method of building and testing.

Running Unit Tests
------------------

With rebar installed, run:

    make test


