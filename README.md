Intro
=====

Vlime is a Common Lisp dev environment for Vim, similar to SLIME for Emacs and
SLIMV for Vim.

It provides REPL integration, as well as omni-completions, cross reference
utilities, a nice inspector, debugger support, and many other great facilities
to aid you in your glorious Common Lisp hacking quest.

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

Must-have:

* Vim 8.0+ and +channel
* ASDF
* Quicklisp
* An Internet connection to install other dependencies from Quicklisp

Note that there was a bug in the channel implementation of Vim, which may
cause large messages to be dropped incorrectly. This was fixed in 8.0.0312,
so using 8.0.0312 or a more recent version is strongly recommended.
[Details](https://groups.google.com/d/topic/vim_dev/Rl0X_R5pjxk/discussion).

Currently Vlime can only detect s-expressions inside parentheses. To make your
life easier, use [paredit](https://github.com/kovisoft/paredit).

Supported CL Implementations
============================

The CL implementations listed below are supported. If you tried out Vlime with
an implementation not listed here, please let me know (see the Contributing
section below for contact info).

```
Implementation  Version  Notes
-----------------------------------------------------
ABCL            1.4.0    Supported by the vlime-patched backend
Allegro CL      10.0     Tested with the Express Edition
CLISP           2.49+    No multithreading support
ECL             16.1.3   No SLDB support
CCL             1.11     
SBCL            1.3.13   
LispWorks       6.1      Tested with the Personal Edition
```

Usage
=====

1. Set up Quicklisp properly.
2. Clone this repo.
3. Install the Vim plugin from Vlime's `vim` directory using your usual way
   (Vundle or something).
4. Run the server: `sbcl --load <vlime repo>/lisp/start-vlime.lisp`

If it's your first time running the server, Vlime will try to install it's
dependencies via Quicklisp.

When the server is up and running, use Vim to start editing a CL source file.

See `:help vlime` for the full documentation.

License
=======

MIT. See `LICENSE.txt`.

Contributing
============

The source repo for Vlime is hosted on GitHub:

    https://github.com/l04m33/vlime

Please send pull requests, and feel free to contact me at l04m33(at)gmail.com
if you have any suggestions for improving Vlime.
