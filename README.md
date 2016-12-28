Intro
=====

Vlime is a Common Lisp dev environment for Vim, similar to SLIME for Emacs and
SLIMV for Vim (And yes, I dug around SLIMV's source code when writing Vlime).

Vlime provides REPL integration inside Vim, as well as omni-completions and
basic SLDB support. And, optionally, it makes you a lot sexier.

Why?
====

* There were barely no choice besides SLIMV.
* SLIMV was written in Python on the Vim side, but I think a Lisp-and-Vimscript
  implementation would be awesome.
* Vim 8 has these nice `channel` APIs, why not try them out?

Current State
=============

Vlime is currently in a pre-alpha state. One should expect changes in APIs and
key bindings etc.

Dependencies
============

* Quicklisp
* libuv
* Vim 8.0+ and +channel
* A decent Common Lisp implementation (SBCL is recommended)

Vlime is built and tested with SBCL, but other implementations may also work.

Currently Vlime can only detect s-expressions inside parentheses. To make your
life easier, use [paredit](https://github.com/kovisoft/paredit).

Installation
============

1. Make sure your system has libuv installed.
2. Set up Quicklisp properly.
3. Clone this repo, and create a symlink to Vlime's `lisp` directory in your
   `local-projects` directory.
4. In your REPL, invoke `(ql:quickload :vlime)`
5. Install the Vim plugin in Vlime's `vim` directory using your usual way
   (Vundle or something).

Usage
=====

Run the server:

    sbcl --load <vlime repo>/lisp/start-vlime.lisp

Use Vim to start editing a Lisp source file. These keys are mapped:

* `<LocalLeader>c`: Connect to Vlime server.
* `<LocalLeader>C`: Switch Vlime connections.
* `<LocalLeader>d`: Disconnect.
* `<LocalLeader>i`: Interaction Mode.
* `<LocalLeader>s`: Describe symbol.
* `<LocalLeader>l`: Load current file.
* `<LocalLeader>m1`: Expand current macro.
* `<LocalLeader>ma`: Expand current macro and all nested macros.
* `<LocalLeader>a`: Disassemble current form.

`<LocalLeader>` defaults to backslash `\`. In addition, you can use `<tab>`
instead of `<c-x><c-o>` to invoke omni-completion.

Most of Vlime's functionalities need an active connection to the server. Press
`<LocalLeader>c` to create one.

In `Interaction Mode` (Entered via `<LocalLeader>i`), you can place the cursor
on an s-expression and then press `<cr>` to send it to the REPL.

In the REPL output buffer, `<c-c>` will interrupt the REPL thread.

When there's an unhandled condition or a thread is interrupted, the SLDB
buffer will appear, with posible restarts and stack frames as it's content.
Pressing `<cr>` on one of the restart options will invoke that restart.
