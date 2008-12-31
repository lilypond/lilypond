\version "2.12.0"
\header {
  texidoc = "Span bars are drawn only between staff bar lines. By setting 
bar lines to transparent, they are shown only between systems.

Setting @code{SpanBar} transparent  removes the barlines
between systems.
 
"
}

\layout {
  \context {
    \Staff
  }
  ragged-right =##t 
}

\relative c' \new StaffGroup <<
  \new Staff {
    a1
    \once \override Score.BarLine #'transparent = ##t
    a1
    \once \override Score.SpanBar #'transparent = ##t
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

