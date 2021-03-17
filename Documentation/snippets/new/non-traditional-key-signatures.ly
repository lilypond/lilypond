\version "2.19.21"

\header {
  lsrtags = "contemporary-notation, pitches, staff-notation, version-specific, non-traditional, world-music"

  texidoc = "
The commonly used @code{\\key} command sets the @code{keyAlterations}
property in the @code{Staff} context.  To create non-standard key
signatures, set this property directly.

The format of this command is a list:

@verbatim
\\set Staff.keyAlterations =
  #`(((octave . step) . alter) ((octave . step) . alter) ...)
@end verbatim

where, for each element in the list @code{octave} specifies the octave
(0 being the octave from middle c to the b above), @code{step} specifies
the note within the octave (0 means c and 6 means b), and @code{alter}
is @code{,SHARP ,FLAT ,DOUBLE-SHARP} etc.

Alternatively, using the more concise format for each item in the list,
@code{(step . alter)} specifies the same alteration holds in all
octaves.  For microtonal scales where a @qq{sharp} is not 100 cents,
@code{alter} refers to the proportion of a 200-cent whole tone.

"
  doctitle = "Non-traditional key signatures"
}

\include "arabic.ly"
\relative do' {
  \set Staff.keyAlterations = #`((0 . ,SEMI-FLAT)
                                 (1 . ,SEMI-FLAT)
                                 (2 . ,FLAT)
                                 (5 . ,FLAT)
                                 (6 . ,SEMI-FLAT))
%\set Staff.extraNatural = ##f
  re reb \dwn reb resd
  dod dob dosd \dwn dob |
  dobsb dodsd do do |
}
