\version "2.13.0"

\header {
  lsrtags = "pitches, staff-notation"
  texidoc = "
The commonly used @code{\\key} command sets the @code{keySignature}
property, in the @code{Staff} context.

To create non-standard key signatures, set this property directly.  The
format of this command is a list:

@code{\\set Staff.keySignature = #`(((octave . step) . alter) ((octave
. step) . alter) ...)} where, for each element in the list,
@code{octave} specifies the octave (0@tie{}being the octave from
middle@tie{}C to the B above), @code{step} specifies the note within the
octave (0@tie{}means@tie{}C and 6@tie{}means@tie{}B), and @code{alter} is
@code{,SHARP ,FLAT ,DOUBLE-SHARP} etc.  (Note the leading comma.)

Alternatively, for each item in the list, using the more concise format
@code{(step . alter)} specifies that the same alteration should hold in all
octaves.

Here is an example of a possible key signature for generating a whole-tone
scale:
"
  doctitle = "Non-traditional key signatures"
}

\relative c' {
  \set Staff.keySignature = #`(((0 . 6) . ,FLAT)
                               ((0 . 5) . ,FLAT)
                               ((0 . 3) . ,SHARP))
  c4 d e fis
  aes4 bes c2
}

