\score{<
  \notes \relative c'' \context Staff=violin{
    \time 3/4
    b4. b8 a g |
    \grace a g8.^\trill fis32 g fis4 a8. a16 |
  }
  \notes \relative c'' \context Staff=violoncello{
    \time 3/4
    \clef tenor

    g8 d^\trill [g d c b] |
    \clef bass b8.^\trill a32 b a4 r |
  }
>
\paper{
  linewidth=-1
}
}

