\version "2.23.14"

\header {
  lsrtags = "paper-and-layout, staff-notation, syntax-and-expressions"

  texidoc = "
This snippet shows how to achieve vertically aligned @code{StaffGroups}
with a @code{SystemStartBar} for each @code{StaffGroup}, but without
connecting them.
"

  doctitle = "Vertical aligned StaffGroups without connecting SystemStartBar"
}


#(set-global-staff-size 18)

\paper {
  indent = 0
  ragged-right = ##f
  print-all-headers = ##t
}

\layout {
  \context {
    \StaffGroup
    \consists Text_mark_engraver
    \consists Staff_collecting_engraver
    systemStartDelimiterHierarchy =
      #'(SystemStartBrace (SystemStartBracket a b))
  }

  \context {
    \Score
    \remove Text_mark_engraver
    \remove Staff_collecting_engraver
    \override SystemStartBrace.style = #'bar-line
    \omit SystemStartBar
    \override SystemStartBrace.padding = #-0.1
    \override SystemStartBrace.thickness = #1.6
    \override StaffGrouper.staffgroup-staff-spacing.basic-distance = #15
  }
}

%%%% EXAMPLE

txt =
\lyricmode {
  Wer4 nur den lie -- ben Gott läßt wal2 -- ten4
  und4 hof -- fet auf ihn al -- le Zeit2.
}

% First StaffGroup "exercise"

eI =
\relative c' {
        \textMark \markup {
                \bold Teacher:
                This is a simple setting of the choral. Please improve it.
                }
        \key a \minor
        \time 4/4
        \voiceOne

        \partial 4
        e4
        a b c b
        a b gis2
        e4\fermata g! g f
        e a a gis
        a2.\fermata
        \bar ":|."
}

eII =
\relative c' {
        \key a \minor
        \time 4/4
        \voiceTwo
        \partial 4
        c4
        e e e gis
        a f e2
        b4 b d d
        c c d d
        c2.
        \bar ":|."
}

eIII =
\relative c' {
        \key a \minor
        \time 4/4
        \clef bass
        \voiceOne

        \partial 4
        a4
        c b a b
        c d b2
        gis4 g g b
        c a f e
        e2.
}

eIV =
\relative c' {
        \key a \minor
        \time 4/4
        \clef bass
        \voiceTwo

        \partial 4
        a,4
        a' gis a e
        a, d e2
        e,4\fermata e' b g
        c f d e
        a,2.\fermata
        \bar ":|."
}

exercise =
\new StaffGroup = "exercise"
<<

  \new Staff
    <<
      \new Voice \eI
      \new Voice \eII
    >>

  \new Lyrics \txt

  \new Staff
    <<
      \new Voice \eIII
      \new Voice \eIV
    >>
>>

% Second StaffGroup "simple Bach"

sbI =
\relative c' {
        \textMark \markup { \bold" Pupil:" Here's my version! }
        \key a \minor
        \time 4/4
        \voiceOne

        \partial 4
        e4
        a b c b
        a b gis2
        e4\fermata g! g f
        e a a gis
        a2.\fermata
        \bar ":|."
}

sbII =
\relative c' {
        \key a \minor
        \time 4/4
        \voiceTwo
        \partial 4
        c8 d
        e4 e e8 f g4
        f f e2
        b4 b8 c d4 d
        e8 d c4 b8 c d4
        c2.
        \bar ":|."
}

sbIII =
\relative c' {
        \key a \minor
        \time 4/4
        \clef bass
        \voiceOne

        \partial 4
        a8 b
        c4 b a b8 c
        d4 d8 c b2
        gis4 g g8 a b4
        b a8 g f4 e
        e2.
}

sbIV =
\relative c' {
        \key a \minor
        \time 4/4
        \clef bass
        \voiceTwo

        \partial 4
        a,4
        a' gis a e
        f8 e d4 e2
        e,4\fermata e' b a8 g
        c4 f8 e d4 e
        a,2.\fermata
        \bar ":|."
}

simpleBach =
\new StaffGroup = "simple Bach"
<<

  \new Staff
    <<
      \new Voice \sbI
      \new Voice \sbII
    >>

  \new Lyrics \txt

  \new Staff
    <<
      \new Voice \sbIII
      \new Voice \sbIV
    >>
>>

% Third StaffGroup "chromatic Bach"

cbI =
\relative c' {
        \textMark \markup {
          \bold "Teacher:"
          \column {
            "Well, you simply copied and transposed a version of J.S.Bach."
            "Do you know this one?"
          }
        }
        \key a \minor
        \time 4/4
        \voiceOne

        \partial 4
        e4
        a b c b
        a b gis4. fis8
        e4\fermata g! g f
        e a a8 b gis4
        a2.\fermata
        \bar ":|."
}

cbII =
\relative c' {
        \key a \minor
        \time 4/4
        \voiceTwo
        \partial 4
        c8 d
        e4 e e8 fis gis4
        a8 g! f!4 e2
        b4 e e d
        d8[ cis] d dis e fis e4
        e2.
        \bar ":|."
}

cbIII =
\relative c' {
        \key a \minor
        \time 4/4
        \clef bass
        \voiceOne

        \partial 4
        a8 b
        c[ b] a gis8 a4 d,
        e8[ e'] d c b4. a8
        gis4 b c d8 c
        b[ a] a b c b b c16 d
        c2.
}

cbIV =
\relative c' {
        \key a \minor
        \time 4/4
        \clef bass
        \voiceTwo

        \partial 4
        a4
        c, e a, b
        c d e2
        e4\fermata e a b8 c
        gis[ g] fis f e dis e4
        a,2.\fermata
        \bar ":|."
}

chromaticBach =
\new StaffGroup = "chromatic Bach"
<<

  \new Staff
    <<
      \new Voice \cbI
      \new Voice \cbII
    >>

  \new Lyrics \txt

  \new Staff
    <<
      \new Voice \cbIII
      \new Voice \cbIV
    >>
>>


% Score

\score {
        <<
        \exercise
        \simpleBach
        \chromaticBach
        >>
        \header {
                title = \markup
                           \column {
                             \combine \null \vspace #1
                             "Exercise: Improve the given choral"
                             " "
                            }
        }
        \layout {
                \context {
                        \Lyrics
                        \override LyricText.X-offset = #-1
                }
        }
}
