
\version "1.3.117";

% Test scm markup text and kerning

% Warning
%  
% This is not a feature, it is a hack.  If you change anything,
% it will probably break (that's because scm markup text is a
% bit broken and needs fixing).  Chances are, it's already
% broken by the time you read this.  Don't complain.
%
% FIXME: put in an item, and typeset by an engraver.

#(define note '(rows (music "noteheads-2" ((kern . -0.1) "flags-stem"))))
#(define eight-note `(rows ,note ((kern . -0.1) (music ((raise . 3.5) "flags-u3")))))
#(define dotted-eight-note `(rows ,eight-note (music "dots-dot")))

\score {
  \notes\relative c'' {
    a1^#`((rows (font-relative-size . -1)) ,dotted-eight-note " = 64")
  }
  \paper {
    linewidth = -1.;
    \translator{
      \ScoreContext
      TextScript \override #'font-shape = #'upright
    }
  }
}
