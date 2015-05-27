\version "2.19.22"
\header{
  texidoc="
Note head shapes may be set from several choices.
The stem endings should be adjusted according to the note head.
If you want different note head styles on one stem,
you must create a special context.

Harmonic notes have a different shape and different
dimensions.
"
}

\layout {
  indent = 0.0
  ragged-right = ##t
}

pattern =
#(define-music-function (name style) (markup? ly:context-mod?)
#{ <<
  s1^#name
  \new Voice \with #style {
    \override Stem.direction = #UP
    e'4 e'2. e'1 e'\breve*1/2 e'\longa*1/4
  }
  \new Voice \with #style {
    \override Stem.direction = #DOWN
    g4 g2. g1 g\breve*1/2 g\longa*1/4
  }
>> #})

patternStyle =
#(define-music-function (style) (symbol?)
  #{
     \pattern #(symbol->string style) \with {
       \override NoteHead.style = #style
     }
  #})

\transpose c c {
  \clef C

  \patternStyle default
  \patternStyle altdefault

  \break

  \patternStyle baroque
  \patternStyle neomensural

  \break

  \patternStyle mensural
  \patternStyle petrucci

  \break

  \patternStyle harmonic
  \patternStyle harmonic-black

  \break

  \patternStyle harmonic-mixed
  \patternStyle diamond

  \break

  \patternStyle cross
  \patternStyle xcircle

  \break

  \patternStyle triangle
  \patternStyle slash

  \break

  \pattern "kievan" \with { \kievanOn }

}



