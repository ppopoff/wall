Wall - An application for posing
================================




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


