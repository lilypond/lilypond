Info for Documentation
----------------------

Current version of the manual: 2.8.0
*** Please update this whenever you run convert-ly on the docs.

convert-ly --from=... --to=... --no-version *.itely
 
%%%%%
Distributions will want to install lilypond.info in postinstall, doing:

    install-info --info-dir=/usr/share/info out/lilypond.info
%%%%%
  * Prepend GNU for dir, must be unique.
  
  * Do not list the `lilypond' node at toplevel, so that `info lilypond'
    goes to Top.

  * List all commands in direntry.
  
@c  * lilypond: (lilypond/lilypond)Running LilyPond.      Invoking the
@c    LilyPond  program.

%%%%%
HINTS FOR STYLE

* Do not forget to create @cindex entries for new sections of text.

* Try not to use punctuation between an introductory sentence and
  display material (music, example code).

* Do not refer to LilyPond in the text.  The reader knows what the
  manual is about.  If you do, capitalization is LilyPond.

* If you explicitly refer to `lilypond', the program (or any other
  command to be executed), say `@command{lilypond}'.

* Do not explicitly refer to the reader/user.  There is no one else
  besides the reader and the writer.

* Do not use abbreviations (don't, won't, etc.).  If you do, use a
  comma after it:

    blabla blabla, i.e., blabla blabla

* Avoid fluff (``Notice that,'' ``as you can see,'' ``Currently,'').

* The above three suggestions refer to the formal Notation Manual
  (chapters 5 and up).  In the Tutorial, Example templates, and
  Putting it all together, you may write more colloquially

* The use of the word `illegal' is inappropriate in most cases.  Say
  `invalid' instead.

* Avoid long stretches of input code.  Noone is going to read them in
  print.  Instead refer to an example input file (@inputfileref), these
  are clickable in HTML.

* Abbrevs in caps, e.g., HTML, DVI, MIDI, etc.

* Colon usage

  0. Do not use a colon to introduce examples, sentences just continue

      in the display material.

  1. To introduce lists
  2. When beginning a quote: "So, he said,..."
     This usage is rarer.  Americans often just use a comma.
  3. When adding a defining example at the end of a sentence.

* To produce good looking texinfo output (for both TTY and DVI) some
  additional formatting rules should be followed.

  . Do not use tabs.  They expand to nothing in DVI output.

  . Do not use spaces at the beginning of a line (except in @example
    or @verbatim environments), and do not use more than a single space
    between words.  `makeinfo' copies the input lines verbatim without
    removing those spaces.

  . Use two spaces after a priod.

  . Variables or numbers which consist of a single character (probably
    followed by a punctuation mark) should be tied properly, either to
    the previous or the next word.  Example:

      The variable@tie{}@var{a} ...

  . To get consistent indentation in the DVI output it is better to avoid
    the @verbatim environment.  Use the @example environment instead if
    possible, but without extraneous indentation.  For example, this

      @example
        foo {
          bar
        }
      @end example

    should be replaced with

      @example
      foo {
        bar
      }
      @end example

    where `@example' starts the line (without leading spaces).

  . Use the `quote' option in @lilypond commands if possible.

  . Do not compress the input vertically; this is, do not use

      Beginning of logical unit
      @example
      ...
      @end example
      continuation of logical unit

    but

      Beginning of logical unit

      @example
      ...
      @end example

      @noindent
      continuation of logical unit

    This makes it easier to not forget `@noindent'.

  . Non-ASCII characters which are in utf-8 should be directly used;
    this is, don't say `Ba@ss{}tuba' but `Baßtuba'.  This ensures that
    all such characters appear in all output formats.

%%%%%



