;;;; interface-description.scm -- part of generated backend documentation
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c)  1998--2004  Han-Wen Nienhuys <hanwen@cs.uu.nl>
;;;;                 Jan Nieuwenhuizen <janneke@gnu.org>


; should include default value?


(ly:add-interface
 'cluster-beacon-interface

 "A place holder for the cluster spanner to determine the vertical
extents of a cluster spanner at this X position.

 "
 '(positions)
 )

(ly:add-interface
 'dynamic-interface
   "Any kind of loudness sign"
   '()
    )

(ly:add-interface
 'finger-interface
 "A fingering instruction"
 '()
 )

(ly:add-interface
 'ligature-interface
 "A ligature"
 '()
 )

(ly:add-interface
 'ligature-bracket-interface
 "A bracket indicating a ligature in the original edition"
 '(width thickness height ligature-primitive-callback))

(ly:add-interface
 'lyric-syllable-interface
 "a single piece of lyrics"
 '())

(ly:add-interface
 'lyric-interface
 "Any object that is related to lyrics."
 '())

(ly:add-interface
 'mark-interface
 "a rehearsal mark"
 '())

(ly:add-interface
 'metronome-mark-interface
 "a rehearsal mark"
 '(
   ))


(ly:add-interface
 'multi-measure-interface
 "Multi measure rest, and friends (mmrest number, mmrest text)."
 '())


(ly:add-interface
'note-name-interface
 "Note name"
 '(style))

(ly:add-interface
 'only-prebreak-interface
 "Kill this grob after the line breaking process."
 '() )

(ly:add-interface
 'piano-pedal-interface
 "A piano pedal sign"
 '())


(ly:add-interface
 'rhythmic-grob-interface
 "Any object with a rhythmic basis. Used to determine which grobs 
are interesting enough to maintain a hara-kiri staff."
 '()
 )

(ly:add-interface
 'stanza-number-interface
 ""
 '()
 )

;;; todo: this is not typesetting info. Move to interpretation.
(ly:add-interface
 'tablature-interface
 "tablature notes"
 '())


;; todo: figure out where  to put this doco:

"
Grob properties form a name space where you can set variables per
object.  Each object however, may have multiple functions. For
example, consider a dynamic symbol, such @code{\ff} (fortissimo). It
is printed above or below the staff, it is a dynamic sign, and it is a
kind of text.

To reflect this different functions of a grob, procedures and variables
are grouped into so-called interfaces.  The dynamic text for example
supports the  following interfaces:
@table @code 
@item font-interface
  The glyph is built from characters from a font, hence the
@code{font-interface}. For objects supporting @code{font-interface}, you
can select alternate fonts by setting @code{font-style},
@code{font-point-size}, etc.

@item dynamic-interface
  Dynamic interface is not associated with any variable or function in
particular, but this makes it possible to distinguish this grob from
other similar grobs (like @code{TextScript}), that have no meaning of
dynamics.

@item text-interface
  This interface is for texts that are to be set using special routines
to stack text into lines, using kerning, etc.

@item general-grob-interface
  This interface is supported by all grob types.
@end table
"
