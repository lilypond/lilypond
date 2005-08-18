\version "2.6.0"
\header {
  texidoc = "Span bars are drawn only between staff bar lines. By setting 
bar lines to transparent, they are shown only between systems.

Setting @code{SpanBar} transparent  removes the barlines
between systems.
 
"
}


\relative c' \new StaffGroup <<
  \new Staff {
    \override Score.BarLine #'transparent = ##t
    a1 a1
    \revert Score.BarLine #'transparent
    \override Score.SpanBar #'transparent = ##t
    a1 a1
  }
  \lyricmode <<
    \new Lyrics { bla1 die bla }
    \new Lyrics { foo bar foo }
  >>
  \new Staff {
    f1 f1 f1 f1
  }
>>
\layout {
  \context {
    \Staff
  }
  raggedright =##t 
}



%% Local variables:
%% LilyPond-indent-level:2
%% End:

