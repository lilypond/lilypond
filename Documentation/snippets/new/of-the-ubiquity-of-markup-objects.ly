\version "2.25.6"

\header {
  categories = "Text"

  texidoc = "
Text objects are entered either as simple strings between double quotes
or as @code{\\markup} blocks that can accept a variety of advanced text
formatting and graphical enhancements.

As such, markup blocks may be used:

@itemize
@item
in any @code{TextScript} object (attached to notes with @code{-},
@code{^} or @code{_}),
@item
in any @code{TextMark} introduced with the @code{\\textMark} keyword,
or @code{\\textEndMark} command, or other similar objects such as
@code{MetronomeMark} introduced with @code{\\tempo},
@item
as standalone markup blocks, entered at the top level outside of any
@code{\\score} block,
@item
in any definition inside the @code{\\header} block (e.g., title,
subtitle, composer) or in some variables defined inside the
@code{\\paper} block such as @code{evenHeaderMarkup} for page numbers.
@end itemize

@code{\\markup} may additionally be used for lyrics, in chord names,
and as dynamics.  In fact, it is possible to use @code{\\markup} to
customize the appearance of virtually any object, as demonstrated in
this example using various methods.
"

  doctitle = "Of the ubiquity of markup objects"
}


\paper {
  paper-width = 8\cm
  paper-height = 8\cm
}

\header {
  title = \markup "Title"
  tagline = \markup "(tagline)"
}

\markup "Top-level markup"

dyn = #(make-dynamic-script #{ \markup \serif "DynamicText" #})

<<
  \new ChordNames \with {
    majorSevenSymbol = \markup "majorSevenSymbol"
  } \chordmode { c1:maj7 }
  \new Staff {
    \tempo \markup "MetronomeMark"
    \textMark \markup "TextMark"

    \once \override TupletNumber.text = \markup "TupletNumber"
      \tuplet 3/2 {
        \once \override NoteHead.stencil = #ly:text-interface::print
        \once \override NoteHead.text = \markup \lower #0.5 "NoteHead"
        c''8^\markup "TextScript"

        \once \override Rest.stencil = #(lambda (grob)
          (grob-interpret-markup grob #{ \markup "Rest" #}))
        r4
      }
  }
  \new Lyrics \lyricmode { \markup "LyricText" 1 }
  \new Dynamics { s1\dyn }
>>
