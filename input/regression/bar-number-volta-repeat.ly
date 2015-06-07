\version "2.19.22"

\header {
  texidoc = "Bar numbers can automatically reset at volta repeats.
"
}

musicWithBarNumberCheck =
#(define-music-function (n)
  (integer?)
#{
  \relative {
    \override Score.BarNumber.break-visibility = ##(#t #t #t)
    \repeat volta 28 {
     c'1 |
     c |
     c |
    }
    \alternative {
        << { c \barNumberCheck #n c } \\ { e e } >>
      {
        c \barNumberCheck #n c |
      }
      {
        c \barNumberCheck #n c |
      }
    }
    c c
  }
#})

{
  \set Score . alternativeNumberingStyle = #'numbers
  \musicWithBarNumberCheck #5
  \musicWithBarNumberCheck #12
}
{ \set Score . alternativeNumberingStyle = #'numbers-with-letters
  \musicWithBarNumberCheck #5
  \musicWithBarNumberCheck #12
}

