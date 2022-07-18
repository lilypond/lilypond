
LilyPond's Feta and Parmesan fonts
==================================

[The name 'Feta' is based on a non-translatable Dutch pun ('font-en-tja').
Given that it is the name of a cheese type, the font for old music symbols is
called 'Parmesan', and the OpenType font that merges these two fonts is called
'Emmentaler'.]

This is a font of music symbols.  All Metafont sources are original.  Most of
the documentation is in comments in the Metafont code.

Non-square pixels are not supported; in other words, the horizontal and vertical
resolution of the output device must be the same.


Conversion from Metafont to PostScript
--------------------------------------

[It is assumed that you have a TeX distribution like TeX Live installed on your
system that provides Metafont and Metapost infrastructure and binaries together
with a very basic plain TeX setup.

For better readability of log files created by Metafont and Metapost it is also
recommended to set

    max_print_line=1000

in your environment.]

The Metafont code gets directly converted to PostScript Type 1 fonts with a
script called `mf2pt1`, which in turn calls `mpost` (the Metapost binary) to
generate outlines and the font editor 'FontForge' to postprocess the output
(mainly to remove outline overlaps and to add hints).

The recommended calling sequence of `mf2pt1` is

    mf2pt1 --rounding=0.0001 <other options> <font>

You need `mf2pt1` version 2.1 or newer.  Note that the LilyPond tarball comes
with a copy of this Perl script (in case your TeX distribution does not
provide it); you can find it in directory `scripts/build`.

Here are some guidelines to assure a clean conversion.

- Never use `---`.  Replace it with `--` together with explicit path
  directions (if necessary).

- Avoid self-intersecting outlines in general since they can confuse
  `mf2pt1`'s algorithm to get the orientation of a closed path.  Note that
  Metapost's implementation of the `turningnumber` primitive (which would
  immediately give the orientation of a path) is severely broken before version
  1.0 of Metapost, thus some hand-made code in `mf2pt1.mp` is used to work
  around this bug.

- If outlines intersect, avoid grazing intersections.  In case two outlines
  intersect in an explicitly defined point, include this point in both
  intersecting paths to avoid problems due to rounding errors.

- Do not use `draw` with a polygonal pen but for straight lines (consisting of
  exactly two points).  In most cases it is quite easy to emulate `draw` with an
  explicit definition of the outline or with `penstroke`.

- Do not apply transformations after calling `fill` – for example, do not
  mirror `currentpicture`.  Instead, transform the path and call `fill`
  afterwards.  This ensures that `mf2pt1` gets the correct outline directions,
  which is a necessary prerequisite for FontForge's algorithm to remove
  overlaps.


Glyph name rules
----------------

- Most glyph names have the form '*group*`.`*name*', where *group* is defined
  with the `fet_begingroup` command, and *name* is given with `fet_beginchar`
  (within a `fet_begingroup` block).  Example: `clefs.vaticana.fa`.

- Sometimes it would be sensible to use negative numbers in glyph names.
  However, the '`-`' character should not be used in a glyph name.  Replace
  it with '`M`'.  For example, write `rests.M3mensural` instead of
  `rests.-3mensural`.

- Glyphs that exist in both an 'up' and 'down' version should start the *name*
  part with either '`u`' or '`d`', respectively.  Examples: `flags.d3`,
  `flags.u3`.  Glyphs that are neutral w.r.t. the direction, and where
  members of the glyph group exist that have 'up' and 'down' versions,
  should start with letter '`s`'.  Example: `noteheads.s0re`.


Design rules
------------

- Always use smooth curve transitions.  Since this is difficult to see in
  Metafont proof sheets (which do not show the tangents) it is recommended to
  call `mf2pt1` as

      FONTFORGE=foo mf2pt1 ...

  ('foo' should be a non-existent program; this avoids the default
  postprocessing).  Then call FontForge to inspect the outlines.

- Use rounded corners.


Hints for stem attachment
-------------------------

Stem attachment of glyphs is controlled by four special variables called
`charwx`, `charwy`, `chardwx`, and `chardwy`.  Stems can be regarded as (very
oblonged) rectangles with slightly rounded corners.  For stems pointing upwards
the lower right corner of this rectangle is attached to the glyph at position
`(charwx, charwy)`.  For stems pointing downwards the position `(chardwx,
chardwy)` is used instead.


Proofing
--------

The proofing tool for Metafont to inspect its output is `gftodvi`, converting a
font in Metafont's native GF font format to a DVI document, which can then be
viewed with DVI viewers like `xdvi`.  The `gftodvi` program needs two special
fonts, 'gray' and 'black'.  Assuming that you are using TeX Live, say

    mktextfm gray
    mktextfm black

on the command line to generate the needed metric files (from its source files),
which are then stored within your local TEXMF tree.

Here is a shell script that you can use to produce two DVI files,
`foo.proof.dvi` and `foo.ljfour.dvi`, showing the glyphs of font `foo.mf` in
proofing mode and a 600dpi rasterization for a LaserJet IV printer.  `mf` is the
name of the Metafont binary.

    #!/bin/sh
    mf "\mode:=proof; input $1"
    gftodvi $1.2602gf \
      && mv $1.dvi $1.proof.dvi \
      && mv $1.log $1.proof.log

    mf "\mode:=ljfour; input $1"
    printf "grayfont black\n" | gftodvi $1.600gf/ \
      && mv $1.dvi $1.ljfour.dvi \
      && mv $1.log $1.ljfour.log

Assuming that you name this script `makefeta.sh`, you can call it with e.g.

    sh makefeta.sh feta23

The main importance of the proofing DVI output is to show points marked in the
Metafont source code with the macros `penlabels` and `labels`.  In most cases,
those points are used for constructing the shapes only and are thus not present
in the output after the conversion with `mf2pt1`.


Rasterization
-------------

Finally, some rules to assure that rasterization at low resolutions (produced
directly with Metafont) gives good results.  Today, this is a minor issue, but
in some cases it might show design flaws.

- Use `define_whole_pixels` and friends where appropriate.

- After calling `set_char_box` to set the metrics of a glyph, the variables
  `b`, `w`, `h`, and `d` hold the breapth, width, height, and depth,
  respectively, scaled to the output device resolution and rounded to
  integer values.  Modify them, if necessary, to ensure symmetrical
  rendering.  However, do not actually *change* them to completely different
  values.

- Use `hround` and `vround` consistently.  A lot of auxiliary macros are defined
  in file `feta-macros.mf`.

- If a path element is duplicated or shifted, use an integer value for the
  offset.

- Add the constant `eps` to mirrored paths to assure correct Metafont
  rasterization.  See the comment and the variables at the end of file
  `feta-params.mf` for details how vertical symmetry should be achieved.


Conversion to OpenType fonts and integration into LilyPond
----------------------------------------------------------

A run of `mf` (or `mf2pt1`) as described above creates a `*.log` file that
contains lines like

    @{group@:rests@}
    ...
    @{char@:breve rest@:39@:0@:2.38@:0@:3.97@:2.38@:0@:0@:3.97@:M1@}
    ...
    @{puorg@:rests@}

These lines are further parsed, see below.  The elements of a 'char' line are as
follows.

  --------------  -----------------------------------------------------------
  *charnamestr*   (Metafont/Metapost glyph description)
  *charbp*        (the glyph 'breapth', i.e., the extent left of the glyph's
                  origin)
  *charwd*        (the glyph width)
  *chardp*        (the glyph depth)
  *charht*        (the glyph height)
  *charwx*        (the x coordinate of the up-stem attachment point)
  *charwy*        (the y coordinate of the up-stem attachment point)
  *chardwx*       (the x coordinate of the down-stem attachment point)
  *chardwy*       (the y coordinate of the down-stem attachment point)
  *idstr*         (the glyph name in the current group)
  --------------  -----------------------------------------------------------

If the 'char' line is within a group, then group name and *idstr* are
concatenated, with a '`.`' inbetween, to form the glyph name.  In the above
example, we are in the group 'rests' and *idstr* is 'M1', thus the glyph name of
'breve rest' is `rests.M1`.

The values in the above table are emitted when `fet_endchar` is called.

The log file contains more data that gets parsed; see file `feta-autometric.mf`
for the details.  For example, lines with the `parameter` keyword hold global
metric information like design size or staff line thickness.

The script `mf-to-table.py` converts the just described information in the
`*.log` file into Lisp files `*.global-lisp` and `*.lisp`.

Finally, the `*.fontforge.py` Python scripts create the 'Emmentaler' and
'Emmentaler Brace' fonts.

- Merge the Feta and Parmesan `*.pfb` font files (created by `mf2pt1`).

- Add OpenType features and kerning to the 'Emmentaler' font.  The necessary
  data is stored in files `emmentaler_*.py`, which are included by
  `gen-emmentaler.fontforge.py`.

- Embed the Lisp data verbatim into the OpenType fonts as SFNT tables 'LILY'
  and 'LILC', respectively.  Data for an additional table called 'LILF' is
  auto-generated.

  For music symbols, LilyPond exclusively uses the data of these three
  tables for glyph metrics information; it does not use the metrics
  information in the font's 'hmtx' table.  On the other hand, script-like
  glyph and text strings are handled by the Pango library, which uses the
  'htmx' and 'kern' tables of the Emmentaler fonts, together with 'GSUB' and
  'GPOS' for advanced OpenType features.


Miscellaneous
-------------

If you add new glyphs, remember to update the following files, which are
not in the `mf` directory.

    scripts/build/generate-encodings.py
    Documentation/en/included/font-table.ly
