\version "2.23.14"

\header {
  lsrtags = "text"

  texidoc = "
Text objects are entered either as simple strings between double quotes
or as @code{\\markup} blocks that can accept a variety of advanced text
formatting and graphical enhancements.

As such, markup blocks may be used:

@itemize
@item
in any TextScript object (attached to notes with @code{-}, @code{^} or
@code{_}),
@item
any @code{TextMark} introduced with the @code{\\textMark} or
@code{\\textEndMark} command, or other similar objects such
as MetronomeMark introduced with @code{\\tempo},
@item
as standalone markup blocks, entered at the top level outside of any
@code{\\score} block,
@item
in any definition inside the @code{\\header} block (e.g. title,
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


%% Thanks to Aaron Hill https://lists.gnu.org/archive/html/lilypond-user/2019-01/msg00437.html

\paper {
  paper-width = 8\cm paper-height = 8\cm
}
\header {
  title = \markup "Header"
  tagline = \markup "(tagline)"
}
\markup "Top-level markup"
dyn = #(make-dynamic-script #{ \markup \text "DynamicText" #})
\score {
  <<
    \new ChordNames
    \with { majorSevenSymbol = \markup "majorSevenSymbol" }
    \chordmode { c1:maj7 }
    \new Staff {
      \tempo \markup "MetronomeMark"
      \textMark "TextMark"
      \once \override TupletNumber.text = \markup "TupletNumber"
      \tuplet 3/2 {
        \once \override NoteHead.stencil = #ly:text-interface::print
        \once \override NoteHead.text = \markup \lower #0.5 "NoteHead"
        c''8^\markup "TextScript"
        \once \override Rest.stencil = #(lambda (grob)
          (grob-interpret-markup grob #{
            \markup  "Rest"
            #}))
        r4
      }
    }
    \new Lyrics \lyricmode { \markup "LyricText" 1 }
    \new Dynamics { s1\dyn }
  >>
}
