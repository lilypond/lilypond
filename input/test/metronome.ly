\version "1.7.6"



% Test scm markup text and kerning

% Warning
%  
% This is not a feature, it is a hack.  If you change anything,
% it will probably break (that's because scm markup text is a
% bit broken and needs fixing).  Chances are, it's already
% broken by the time you read this.  Don't complain.
%
% FIXME: put in an item, and typeset by an engraver.

eigthStem = \markup \combine
	\musicglyph #"flags-stem"
	\translate #'(0.0 . 3.5) \musicglyph #"flags-u3"
eighthNote = \markup
	\override #'(word-space . 0.0)
	{ \musicglyph #"noteheads-2"
	  \translate #'(-0.05 . 0.1) \eigthStem }

\score {
  \notes\relative c'' {
    a1^\markup { \magnify #0.9 \eighthNote " = 64" }
  }
  \paper {
    linewidth = -1.
    \translator{
      \ScoreContext
      TextScript \override #'font-shape = #'upright
    }
  }
}
%% new-chords-done %%
