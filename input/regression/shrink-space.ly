\version "2.25.10"

\header {
  texidoc = "The space types @code{shrink-space} and
@code{semi-shrink-space} only shrink."
}

\paper {
  ragged-bottom = ##t
  left-margin = 1.8\cm
  right-margin = 1.8\cm
  #(set-paper-size "a5")
  tagline = ##f
  print-first-page-number = ##t
}

\layout {
  indent = 0\mm
}


A = {
  \key ges \major
  \omit Staff.TimeSignature
  \repeat unfold 16 f16 \noBreak
  \repeat unfold 16 f16 \break

  f4 \key c\major f \key es \major
    f \key c\major f \key es \major \noBreak
  f4 \key c\major f \key es \major
    f \key c\major f \key es \major \break

  f2 f
}

B = {
  \tuplet 17/16 \repeat unfold 17 f16 \noBreak
  \tuplet 17/16 \repeat unfold 17 f16 \break
  \repeat volta 2 {
    \tuplet 17/16 \repeat unfold 17 f16 \noBreak
    \tuplet 17/16 \repeat unfold 17 f16 \break }

  f2 f
}


\book {
  \markup "fixed-space"
  \relative c' {
    \override Staff.KeySignature.space-alist.first-note =
                #'(fixed-space . 2.5)
    \override Staff.KeyCancellation.space-alist.first-note =
                #'(fixed-space . 2.5)
    \A
  }

  \markup "fixed-space"
  \relative c' {
    \override Staff.TimeSignature.space-alist.first-note =
                #'(fixed-space . 2.0)
    \override Staff.BarLine.space-alist.first-note =
                #'(fixed-space . 1.3)
    \B
  }

  \pageBreak

  \markup "shrink-space"
  \relative c' {
    \override Staff.KeySignature.space-alist.first-note =
                #'(shrink-space . 2.5)
    \override Staff.KeyCancellation.space-alist.first-note =
                #'(shrink-space . 2.5)
    \A
  }

  \markup "semi-shrink-space"
  \relative c' {
    \override Staff.TimeSignature.space-alist.first-note =
                #'(semi-shrink-space . 2.0)
    \override Staff.BarLine.space-alist.first-note =
                #'(semi-shrink-space . 1.3)
    \B
  }
}
