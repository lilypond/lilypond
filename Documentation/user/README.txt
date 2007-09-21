Info for Documentation
----------------------

%%%%% UPDATING DOCS
convert-ly -e --from=... --to=... --no-version *.itely

% to find the current version number,
grep "version \"" tutorial.itely

%  (nobody ever remembers to update this file, so I've stopped
%  trying to record it here)


%%%%% BOOKS

There are three parts to the documentation: the Learning Manual,
the Notation Reference, and the Technical Details.

* Learning Manual: long, chatty, friendly explanations go here.
This is aimed at users learning something for the first time --
not necessarily just learning lilypond notation, but also things
like learning how to deal with projects, tweaking, preparing parts
for orchestras, etc.  Less formal language may be used here.

Users are encouraged to read the complete Learning Manual from
start-to-finish.


* Notation Reference: a (hopefully complete) description of
LilyPond input notation.  Some material from here may be
duplicated in the Learning Manual (for teaching).  The material is
presented in an approximate order of increasing difficulty, but
the goal is _not_ to provide a step-by-step learning environment.
For example, all material under "Pitches" should remain in that
section, even though microtonal accidentals may seem more advanced
than info about clefs or time signatures -- "Pitches" should be a
one-stop reference about the pitch portion of notes.  This section
is written in formal technical writing style.

Users are not expected to read this manual from start to finish.
However, they should be familiar with the material in the Learning
Manual (particularly ``Fundamental Concepts''), so do not repeat
that material in this book.


* Program Usage: information about using the program lilypond with
other programs (lilypond-book, operating systems, GUIs,
convert-ly, etc).  This section is written in formal technical
writing style.

Users are not expected to read this manual from start to finish.


%%%%% SECTION ORGANIZATION

The order of headings inside documentation sections should be:

main docs
@refcommands
@commonprop
@seealso
@refbugs

(omit any headings which do not apply)

Including at least one link to @lsrdir{} inside @seealso is
highly recommended.


%%%%% FORMATTING

* Use two spaces for indentation in lilypond examples.

* Do not use tabs.  They expand to nothing in DVI output.

* Do not use spaces at the beginning of a line (except in @example
  or @verbatim environments), and do not use more than a single
  space between words.  `makeinfo' copies the input lines verbatim
  without removing those spaces.

* Use two spaces after a period.

* Don't use a @ref{link to another section} in the middle of a
  sentence.  It looks ok in HTML, moderately bad in PDF, and
  utterly horrible in INFO.  Instead, reword the sentence so that
  users are encouraged to see @ref{link to another section}.
  (at the end of the sentence)

* Variables or numbers which consist of a single character
  (probably followed by a punctuation mark) should be tied
  properly, either to the previous or the next word.  Example:

      The variable@tie{}@var{a} ...

* To get consistent indentation in the DVI output it is better to
  avoid the @verbatim environment.  Use the @example environment
  instead if possible, but without extraneous indentation.  For
  example, this

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

* Use the `quote' option in @lilypond commands if possible.

  Do not compress the input vertically; this is, do not use

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

  This makes it easier to avoid forgetting the `@noindent'.

* Non-ASCII characters which are in utf-8 should be directly used;
  this is, don't say `Ba@ss{}tuba' but `Baßtuba'.  This ensures that
  all such characters appear in all output formats.

* Lines should be less than 72 characters long.  (I personally
  recommend writing with 66-char lines, but don't bother modifying
  existing material.)

* Use @q instead of `...' and @qq instead of ``...''.  The latter macro
  should be used with care since we use `...' as the default quoting
  throughout the manual, except for things related to direct speech.

  Warning: @q{} and @qq{} *MUST NOT* come at the end or beginning
  of a line (unless it begins the paragraph).  If these macros
  @qq{appear like this}, then they will being a new paragraph.

  In most cases, you should use @code{} or @samp{} instead.


%%%%% READABILITY

* LilyPond input should be produce via
    @lilypond[verbatim,quote,ragged-right]
  with `fragment' and `relative=2' optional.

  Examples about page layout may alter the quote/ragged-right
  options.  Omitting `verbatim' is not recommended (other than the
  ``inspirational headword'' examples)

* Do not forget to create @cindex entries for new sections of text.
  Enter commands with @funindex, i.e.
    @cindex pitches, writing in different octaves
    @funindex \relative
  do not bother with the @code{} (they are added automatically).  These
  items are added to both the command index and the unified index.

* Avoid long stretches of input code.  Noone is going to read them in
  print.  Instead refer to an example input file (with @lsr{}); these
  are clickable in HTML.

* Abbrevs in caps, e.g., HTML, DVI, MIDI, etc.

* Colon usage

  1. To introduce lists
  2. When beginning a quote: "So, he said,..."
     This usage is rarer.  Americans often just use a comma.
  3. When adding a defining example at the end of a sentence.


%%%%% HINTS FOR TECHNICAL WRITING STYLE

* Do not refer to LilyPond in the text.  The reader knows what the
  manual is about.  If you do, capitalization is LilyPond.

* If you explicitly refer to `lilypond' the program (or any other
  command to be executed), say `@command{lilypond}'.

* Do not explicitly refer to the reader/user.  There is no one
  else besides the reader and the writer.

* Do not use abbreviations (don't, won't, etc.).  If you do, use a
  comma after it:

    blabla blabla, i.e., blabla blabla

* Avoid fluff (``Notice that,'' ``as you can see,''
  ``Currently,'').

* The use of the word `illegal' is inappropriate in most cases.
  Say `invalid' instead.



