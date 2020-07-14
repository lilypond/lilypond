\version "2.21.3"

\header {
  texidoc = "A @code{DurationLine} grob may end with a special behaviour.
Currently available are hooks (with settable direction) and arrows."
}

%% Running to the very end of a score, the DurationLine is the not-last
%% part of a broken spanner, thus we need to go for the
%% right-broken-subproperty:
%% \override DurationLine.bound-details.right-broken.end-style = #'arrow/hook
%% or
%% call the function lastEndStyle with 'arrow/hook

lastEndStyle =
#(define-music-function (end-style)(symbol?)
#{
  \override DurationLine.after-line-breaking =
    #(lambda (grob)
      (let* ((orig (ly:grob-original grob))
             (siblings (if (ly:grob? orig) (ly:spanner-broken-into orig) '()))
             (last-grob (if (pair? siblings) (last siblings) #f)))
        (if last-grob
            (ly:grob-set-nested-property!
              last-grob
              '(bound-details right-broken end-style) end-style))))
#})

\layout {
  \context {
    \Voice
    \consists "Duration_line_engraver"
    \omit Stem
    \omit Flag
    \omit Beam
    \override NoteHead.duration-log = 2
  }
}

\score {
  \new Voice = "main"
  {
    b1\-
    <<
      \context Voice = "foo" {
        \voiceOne
        d''2\-
      }
      \context Voice = "bar" {
        \voiceTwo
        d'2\-
        \oneVoice
      }
    >>
    << { d''\- } \\ d'\- >>
    \lastEndStyle #'arrow
    e'\-
    \bar "|."
  }
  \layout {
    \context {
      \Voice
      \override DurationLine.bound-details.right.end-style = #'arrow
    }
  }
}

\score {
  \new Voice = "main"
  {
    b1\-
    <<
      \context Voice = "foo" {
        \voiceOne
        \override DurationLine.details.hook-direction = #DOWN
        d''2\-
      }
      \context Voice = "bar" {
        \voiceTwo
        d'2\-
        \oneVoice
      }
    >>
    << { d''\- } \\ d'\- >>
    \lastEndStyle #'hook
    e'\-
    \bar "|."
  }
  \layout {
    \context {
      \Voice
      \override DurationLine.bound-details.right.end-style = #'hook
    }
  }
}