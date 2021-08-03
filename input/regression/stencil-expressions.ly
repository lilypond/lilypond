\header {
  title = "Lilypond Stencil Tests"
  author = "The Author"
  subject = "The Subject"
  texidoc = "Tests all lilypond stencil commands that are relevant to PDF output"
  keywords = "Lilypond, Cairo, experimental, patch, stencil command tests"
}

\version "2.23.4"

\paper {
  #(set-paper-size "a4")
  top-margin = 10\mm
  bottom-margin = 10\mm
  left-margin = 10\mm
  line-width = 190\mm
}

global = { \time 6/8  \key c \minor }
#(set-global-staff-size 19)

\bookpart{
  \markup {
    \center-column {
      \abs-fontsize #30 \line { LilyPond }
      \abs-fontsize #20 \line {
        \scale #'(-1 . 1)
        "stencil tests"
      }
      \abs-fontsize #20 \line {
        \with-color #(x11-color 'blue)
          \scale #'(1 . -1)
            "LilyPond stencil test page"
      }
      \vspace #15
      \abs-fontsize #18 \line {
        "Lilypond stencil test page"
        \concat {
          \with-dimensions #'(0 . 0) #'(0 . 0)
            \with-color #(rgb-color 1.0 0.0 0.0 0.15)
              \draw-circle #45 #0 ##t
          \with-dimensions #'(0 . 0) #'(0 . 0)
            \with-color #(rgb-color 0.8 0.0 0.2 0.15)
              \draw-circle #40 #0 ##t
          \with-dimensions #'(0 . 0) #'(0 . 0)
            \with-color #(rgb-color 0.6 0.0 0.4 0.15)
              \draw-circle #35 #0 ##t
          \with-dimensions #'(0 . 0) #'(0 . 0)
            \with-color #(rgb-color 0.4 0.0 0.6 0.15)
              \draw-circle #30 #0 ##t
          \with-dimensions #'(0 . 0) #'(0 . 0)
            \with-color #(rgb-color 0.2 0.0 0.8 0.15)
              \draw-circle #25 #0 ##t
          \with-dimensions #'(0 . 0) #'(0 . 0)
            \with-color #(rgb-color 0.0 0.0 1.0 0.15)
              \draw-circle #20 #0 ##t
        }
        "LilyPond stencil test page"
      }
      \vspace #1
      \line {
        \draw-circle #3 #0.5 ##f
        \hspace #2
        \scale #'(2 . 1) \circle { Foobar }
        \hspace #2
        \draw-circle #3 #0 ##t
      }
      \vspace #1
      \line {
        \postscript " save restore "
        \rotate #-40 \ellipse { Foobar }
        \hspace #2
        \ellipse { "Dr. Au Thor" }
        \hspace #2
        \rotate #20 \rotate #20 \oval { Foobar }
      }
      \vspace #1
      \line {
        \rotate #-30 "-30"
        \rotate #0 "000"
        \rotate #30 "030"
        \rotate #60 "060"
        \rotate #90 "090"
        \rotate #120 "120"
        \rotate #150 "150"
        \rotate #180 "180"
        \rotate #210 "210"
        \rotate #240 "240"
        \rotate #270 "270"
        \rotate #300 "300"
        \rotate #330 "330"
        \rotate #360 "360"
        \rotate #390 "390"
      }
      \vspace #1
      \line {
        \page-link #2 \italic "This links to page 2..."
      }
      \vspace #1
      \line {
        \woodwind-diagram #'bassoon #'()
        \woodwind-diagram #'flute #'((cc . (one1q))
                                     (lh . ())
                                     (rh . ()))
        \woodwind-diagram #'flute #'((cc . (one1h))
                                     (lh . ())
                                     (rh . ()))
        \woodwind-diagram #'flute #'((cc . (one3q))
                                     (lh . ())
                                     (rh . ()))
        \woodwind-diagram #'flute #'((cc . (oneR))
                                     (lh . ())
                                     (rh . ()))
        \woodwind-diagram #'flute #'((cc . (oneF two))
                                     (lh . ())
                                     (rh . ()))
      }
      \vspace #1
      \line {
        \override #'((on . 5.0)
                     (off . 5.0)
                     (phase . 0.0)
                     (full-length . #f))
          \draw-dashed-line #'(30 . 0)
        \override #'((on . 1.0)
                     (off . 0.5)
                     (phase . 0.0)
                     (full-length . #f))
          \draw-dashed-line #'(30 . 0)
      }
      \vspace #-0.95
      \line {
        \override #'((on . 5.0)
                     (off . 5.0)
                     (phase . 5.0)
                     (full-length . #f))
          \draw-dashed-line #'(30 . 0)
        \override #'((on . 1.0)
                     (off . 0.5)
                     (phase . .75)
                     (full-length . #f))
          \draw-dashed-line #'(30 . 0)
      }
      \vspace #1
      \line {
        "Test the textedit links in the score below ..."
      }
      \vspace #1
      \score {
        <<
          \new Staff \with {
            \magnifyStaff #2/3
            instrumentName = "Violin"
          }
          \relative {
            \override NoteHead.color = "lightsalmon"
            \override Flag.color = "#E30074"
            \override Beam.color = "#5e45ad"
            \override Rest.color = "#3058"
            \global
            c'8.(\f^> b16 c d) ees8.(^> d16 c b)
            g8.(^> b16 c ees) g8-.^> r r
            R2.
          }
          \new PianoStaff \with {
            instrumentName = "Piano"
          }
          <<
            \new Staff \relative {
              \global
              s2.
              s4. s8 r8 r16 <c' f aes c>
              <c f aes c>4.^> <c ees g>8 r r
            }
            \new Staff \relative {
              \global
              \clef "bass"
              <<
                {
                  \once \override DynamicText.X-offset = #-3
                  <ees g c>2.~->^\f
                  <ees g c>4.~ <ees g c>8
                }
                \\
                {
                  <c g c,>2.~
                  <c g c,>4.~ <c g c,>8
                }
              >>
              r8 r16 <f, c' aes'>16
              <f c' aes'>4.-> <c' g'>8 r r
            }
          >>
        >>
      }
    }
  }
}
\bookpart {
  \markup {
    \center-column {
      \fill-line { \page-link #1 \italic "This links to page 1..." }
      \fill-line { \page-link #3 \italic "This links to page 3..." }
    }
  }
}

\bookpart  {
  \markup {
    \center-column {
      \fill-line { \page-link #2  \italic "This links to page 2..." }
    }
  }
}
