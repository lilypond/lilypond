\version "2.3.8"
\header { texidoc = "@cindex Scheme Interactions
Using @code{ly:export}, the result of Scheme expressions can be passed
as LilyPond input.  Within a Scheme expression, you can use, define or 
change the corresponding variables. In this example, the D-s and E-s are 
generated using scheme functions, and between there are manually 
(without Scheme) entered C-s.

" }

foo =  \transpose c c { d''4-. }
bra =  \transpose c c { e'4-. }


\score { 
  \context Voice \relative c' {
      c4
      #(ly:export (make-sequential-music (list foo foo foo )))
      #(begin (define baz (make-simultaneous-music (list foo bra)))
	(empty-music))
      c4
      \baz
      c4
  }
	\paper {raggedright=##t}
}

