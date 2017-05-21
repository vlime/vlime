Intro
=====

Vlime is a Common Lisp dev environment for Vim (and Neovim), similar to SLIME
for Emacs and SLIMV for Vim.

It provides REPL integration, as well as omni-completions, cross reference
utilities, a nice inspector, debugger support, and many other great facilities
to aid you in your glorious Common Lisp hacking quest.

Short demo:

[![asciicast](https://asciinema.org/a/106146.png)](https://asciinema.org/a/106146)

Why?
====

* There were barely no choice besides SLIMV.
* SLIMV was written in Python on the Vim side, but I think a Lisp-and-Vimscript
  implementation would be awesome.
* Vim 8 has these nice `channel` APIs, why not try them out?

How Does It Work?
=================

Vlime consists of a server written in Common Lisp, and a client written in
Vimscript.

The server is basically a wrapped Swank server. The extra wrapper code
translates the messages from JSON to Swank commands, and vise versa.

The client handles user input, emits JSON messages, and communicates with the
server via Vim channels, or Neovim Jobs.

Current State
=============

Vlime is currently in beta state. Please beware of bugs, and file an issue if
you find anything weird/unexpected (see the Contributing section below).

Dependencies
============

Must-have:

* Vim 8.0+ with +channel feature, or Neovim 0.2.0+ and [ncat command](https://nmap.org/book/install.html)
* ASDF
* [Quicklisp](https://www.quicklisp.org/beta/#installation)
* An Internet connection to install other dependencies from Quicklisp

Note that there was a bug in the channel implementation of Vim, which may
cause large messages to be dropped incorrectly. This was fixed in 8.0.0312,
so using 8.0.0312 or a more recent version is strongly recommended.
[Details](https://groups.google.com/d/topic/vim_dev/Rl0X_R5pjxk/discussion).

When running inside Neovim, Vlime relies on the [ncat](https://nmap.org/ncat/)
command (or other netcat clones) to make connections to the server. You'll need
to have that command available in your `$PATH`. This is because Neovim doesn't
support making non-messagepack socket connections
[yet](https://github.com/neovim/neovim/pull/6594#issuecomment-298851709).

Currently Vlime can only detect s-expressions inside parentheses. To make your
life easier, use [paredit](https://github.com/kovisoft/paredit) or
[vim-surround](https://github.com/tpope/vim-surround).

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
   for example, install current plugin by dein:
   ```vim
   call dein#add('l04m33/vlime', {'on_ft' : 'lisp', 'rtp': 'vim'})
   ```
4. Run the server: `sbcl --load <vlime repo>/lisp/start-vlime.lisp`
5. When the server is up and running, use Vim to start editing a CL source file,
   and type "\cc" (without the quote marks) in normal mode to connect to the
   server.

If it's your first time running the server, Vlime will try to install it's
dependencies via Quicklisp.

You can also let Vim start the server for you. See `:help vlime-start-up`.

See `:help vlime-tutor` for a tutorial on how to use the main features, and
`:help vlime` for the full documentation.

License
=======

MIT. See `LICENSE.txt`.

Contributing
============

The source repo for Vlime is hosted on GitHub:

    https://github.com/l04m33/vlime

Issues and pull requests are welcome. Please feel free to contact me at
l04m33(at)gmail.com if you have any suggestions for improving Vlime.
