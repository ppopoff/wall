Wall - An application for posing
================================
A small chat server that works as a wall. Messages are coming in
and staying inside the wall.

Technical description is [here](desc):

[desc]: https://docs.google.com/document/d/12XHhVOO1VPjPcq7C2FJhAXJMxMoD_UkPavYiCIsFMnM/edit?usp=sharing




The structure is following OTP Design principles

## Folder structure
  * ebin - should contain .beam files corresponding to the
    Erlang source files. In addition it contains `ebin/myapp.app`
    file.
  * include
  * priv - different scripts
  * src - source code

## 'src' folder
  * src/wall.app.src
  * src/wall_app.erl
  * src/wall_sup.erl


Rebar is also included

To compile the appilcation please use

    rebar compile

To remove artifacts from previous compilation use

    rebar clean

To run unit-tests that are now are not written

    rebar eunit

To generate the documentatiion using EDoc

    rebar doc


