\version "2.1.26"
\header { texidoc = "@cindex Scheme Interactions
With @code{ly:export}, you can pass of the result of
Scheme expressions as lilypond input.  Within a Scheme expression,
you can use, define or change lilypond variables.

In this example, the E-s and D-s are generated using scheme functions,
and woven together with manually entered C-s.

" }

foo = \notes \transpose c c { d''4-. }
bra = \notes \transpose c c { e'4-. }


\score { 
  \context Voice \notes\relative c' {
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

