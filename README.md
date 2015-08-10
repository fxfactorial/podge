**Podge** is a centralization of helper functions and shortcuts that I
have frequently found myself writing over and over again in [OCaml](http://www.ocaml.org). It
doesn't depend on Jane Street's [Core](https://github.com/janestreet/core), [Batteries](https://github.com/ocaml-batteries-team/batteries-included) or even [Lwt](http://ocsigen.org/lwt/). Rather
Podge picks among various existing smaller packages that you probably
already have installed and provides helper functions for common tasks
related to usages of those libraries. Podge also provides some extra
modules like the Math module.

**Podge** is well suited for hackathons, especially when you just want
to do a quick HTTP get request for JSON data and subsequently play
with the JSON.

For example, here is a self-contained demonstration program that does
a `HTTP` get request

```ocaml
(* This file is named show_query.ml *)
#require "podge"

let () =
  Podge.Web.get Sys.argv.(1)
  |> Podge.Yojson.show_pretty_of_in_mem
```

And you can run it from the command line with: 
```shell
$ utop show_query.ml http://google.com
{
  "headers": [
    "HTTP/1.1 301 Moved Permanently", "Location: http://www.google.com/",
    "Content-Type: text/html; charset=UTF-8",
    "Date: Mon, 10 Aug 2015 19:20:56 GMT",
    "Expires: Wed, 09 Sep 2015 19:20:56 GMT",
    "Cache-Control: public, max-age=2592000", "Server: gws",
    "Content-Length: 219", "X-XSS-Protection: 1; mode=block",
    "X-Frame-Options: SAMEORIGIN", "Alternate-Protocol: 80:quic,p=0"
  ],
  "body":
    "<HTML><HEAD><meta http-equiv=\"content-type\" content=\"text/html;charset=utf-8\">\n<TITLE>301 Moved</TITLE></HEAD><BODY>\n<H1>301 Moved</H1>\nThe document has moved\n<A HREF=\"http://www.google.com/\">here</A>.\r\n</BODY></HTML>\r\n"
}
```

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

I focus on functionality, not new data structures or improvements of the
StdLib provided data structures, functions.

Perhaps there will be something of use in here as well for you. I
hope that some things here will help newcomers to the language as
well or at least help with quick Python like prototyping.