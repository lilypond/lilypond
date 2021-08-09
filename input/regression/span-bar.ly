\version "2.19.21"
\header {
  texidoc = "Span bars are drawn only between staff bar lines.  By setting
bar lines to transparent, they are shown only between systems.

Setting @code{SpanBar} transparent removes the bar lines
between systems.
 
"
}

\layout {
  \context {
    \Staff
  }
  ragged-right =##t 
}

\relative \new StaffGroup <<
  \new Staff {
    a1
    \once \hide Score.BarLine
    a1
    \once \hide Score.SpanBar
    a1 a1
    \bar "|."
  }
  \lyricmode <<
    \new Lyrics { bla1 die bla }
    \new Lyrics { foo bar foo }
  >>
  \new Staff {
    f1 f1 f1 f1
  }
>>



%% Local variables:
%% LilyPond-indent-level:2
%% End:

