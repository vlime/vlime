Intro
=====

Vlime is a Common Lisp dev environment for Vim (and Neovim), similar to SLIME
for Emacs and SLIMV for Vim.

It provides REPL integration, as well as omni-completions, cross reference
utilities, a nice inspector, debugger support, and many other great facilities
to aid you in your glorious Common Lisp hacking quest.

To get your feet wet: [Quickstart](#quickstart)

Short demo:

[![asciicast](https://asciinema.org/a/129756.png)](https://asciinema.org/a/129756)

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

* Vim 8.0.0312+ with +channel, or Neovim 0.2.2+
* ASDF
* Quicklisp
* An Internet connection to install other dependencies from Quicklisp

Note that there was a bug in the channel implementation of Vim, which may
cause large messages to be dropped incorrectly. This was fixed in 8.0.0312.
[Details](https://groups.google.com/d/topic/vim_dev/Rl0X_R5pjxk/discussion).

Currently Vlime can only detect s-expressions inside parentheses. To make your
life easier, use [parinfer](https://github.com/bhurlow/vim-parinfer) or
[paredit](https://github.com/kovisoft/paredit).

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

Quickstart
==========

Before proceeding with the instructions shown below, please make sure
[Quicklisp](https://www.quicklisp.org/beta/#installation) is properly installed.

Installing using [Vundle](https://github.com/VundleVim/Vundle.Vim):

1. Add `Plugin 'l04m33/vlime', {'rtp': 'vim/'}` to your `vimrc`, then run
   `:PluginInstall` in Vim.
2. Run the server: `sbcl --load <your bundle dir>/vlime/lisp/start-vlime.lisp`

Installing using Vim-Plug

1. Add `Plug 'l04m33/vlime', {'rtp': 'vim/'}` to your `vimrc`, then run
   `:PlugInstall` in Vim.
2. Run the server: `sbcl --load <your bundle dir>/vlime/lisp/start-vlime.lisp`

Installing using [dein.vim](https://github.com/Shougo/dein.vim):

1. Add `call dein#add('l04m33/vlime', {'rtp': 'vim/'})` to your `vimrc`, then run
   `:call dein#install(['vlime'])` in Vim.
2. Run the server:
   `sbcl --load <your bundle dir>/repos/github.com/l04m33/vlime/lisp/start-vlime.lisp`

Installing manually:

1. Clone this repo.
2. Make sure the `<vlime repo>/vim/` directory is in your `runtimepath` (see
   `:help rtp`). You may use symlinks to point to this directory, but please
   don't move it from the Vlime source tree, or Vlime may not be able to
   automatically locate the server entry point.
3. Run the server: `sbcl --load <vlime repo>/lisp/start-vlime.lisp`

If it's your first time running the server, Vlime will try to install it's
dependencies via Quicklisp.

When the server is up and running, use Vim to start editing a CL source file,
and type "\cc" (without the quote marks) in normal mode to connect to the
server.

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

See `:help vlime-tests` for a how-to on setting up and running the
tests for development.

Sponsor
=======

<a target='_blank' rel='nofollow' href='https://app.codesponsor.io/link/EFJRj73XqnJXrjmRNJd9gKeU/l04m33/vlime'>
  <img alt='Sponsor' width='888' height='68' src='https://app.codesponsor.io/embed/EFJRj73XqnJXrjmRNJd9gKeU/l04m33/vlime.svg' />
</a>
