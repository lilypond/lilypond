
;; todo: move this to engraver-init.ly 

(define context-description-alist
  '(
    (Grace . "
    The context for handling grace notes.  It used to be instantiated
    automatically when you use @code{\grace}.  Basically, it is an
    `embedded' miniature of the Score context.  Since this context
    needs special interaction with the rest of LilyPond, you should
    not explicitly instantiate it.

   DEPRECATED.
")
    (LyricsVoice . "
    Corresponds to a voice with lyrics.  Handles the printing of a
    single line of lyrics.
")
    (Thread . "
    Handles note heads, and is contained in the Voice context.  You
    have to instantiate this explicitly if you want to adjust the
    style of individual note heads.
")
    (Voice . "
    Corresponds to a voice on a staff.  This context handles the
    conversion of dynamic signs, stems, beams, super- and subscripts,
    slurs, ties, and rests.

    You have to instantiate this explicitly if you want to have
    multiple voices on the same staff.")

    (ChordNamesVoice . "
    A voice with chord names.  Handles printing of a line of chord
    names.")

    (ChordNames . "
    Typesets chord names.  Can contain @code{ChordNamesVoice}
    contexts.")

    (Lyrics . "
    Typesets lyrics.  It can contain @code{LyricsVoice} contexts.
")
    (Staff . "
    Handles clefs, bar lines, keys, accidentals.  It can contain
    @code{Voice} contexts.
")
    (RhythmicStaff . "
    A context like @code{Staff} but for printing rhythms.  Pitches are
    ignored; the notes are printed on one line.  It can contain
    @code{Voice} contexts.
")
    (GrandStaff . "
    Contains @code{Staff} or @code{RhythmicStaff} contexts.  It adds a
    brace on the left side, grouping the staves together.  The bar
    lines of the contained staves are connected vertically.  It can
    contain @code{Staff} contexts.")

    (PianoStaff . "
    Just like @code{GrandStaff} but with @code{minVerticalAlign} set
    equal to @code{maxVerticalAlign} so that interstaff beaming and
    slurring can be used.")

    (StaffGroup . "
    Contains @code{Staff} or @code{RhythmicStaff} contexts.  Adds a
    bracket on the left side, grouping the staves together.  The bar
    lines of the contained staves are connected vertically.  It can
    contain @code{Staff}, @code{RhythmicStaff}, @code{GrandStaff}, or
    @code{Lyrics} contexts.
")
    (ChoirStaff . "
    Identical to @code{StaffGroup} except that the contained staves
    are not connected vertically.
")
    (Score . "
    This is the top level notation context.  No other context can
    contain a @code{Score} context.  This context handles the
    administration of time signatures.  It also makes sure that items
    such as clefs, time signatures, and key-signatures are aligned
    across staves.  It can contain @code{Lyrics}, @code{Staff},
    @code{RhythmicStaff}, @code{GrandStaff}, @code{StaffGroup}, and
    @code{ChoirStaff} contexts.

    You cannot explicitly instantiate a Score context (since it is
    not contained in any other context).  It is instantiated
    automatically when an output definition (a @code{\score} or
    @code{\paper} block) is processed.
")
    )
  )

(set! context-description-alist
      (sort context-description-alist alist<?))
