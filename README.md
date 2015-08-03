**Podge** is a centralization of helper functions and shortcuts that 
I have frequently found myself writing over and over again in
[OCaml](http://www.ocaml.org). It doesn't depend on Jane Street's [Core](https://github.com/janestreet/core) library, nor does it
depend on [Batteries](https://github.com/ocaml-batteries-team/batteries-included). Rather Podge picks among various existing smaller
packages and provides helper functions for common tasks related to
usages of those libraries. Podge also provides some extra modules,
like the Math module. 

# Overview of Modules

Everything is contained under one module, the `Podge` module. Modules
that contain helpers for existing OCaml packages will have the same
name as the package, for example `Podge.Yojson` contains functions for
working with the `yojson` package. While `Podge.Math` contains various
mathematical and statistical functions.

Probably the easiest way to learn what's provided by Podge is to look
at it via `ocp-browser`, provided by the [ocp-index](https://github.com/OCamlPro/ocp-index) package, have
`lambda-term` installed before you install `ocp-index` so that
`ocp-browser` is installed.

![img](./podge_listing.gif)

Hopefully the functions are named in such a way that you can infer the
semantics/intended usage.

# Yet another Standard Library Replacement?

No, this isn't yet another attempt at a standard library
replacement. Rather this is one place for me to put all code that I've
had scattered all around my hard-drive ranging from stuff that I've
written, to useful StackOverflow answers, to IRC chats, general
googling and Computer Science courses.

I focus on functions, not new data structures or improvements of the
StdLib provided data structures or functions.

Perhaps there will be something of use in here as well for you. (I
hope that some things here will help newcomers to the language as
well)