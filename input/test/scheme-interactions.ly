\version "1.7.3"

\header {

    texidoc = "With @code{ly:export}, you can pass of the result of
Scheme expressions as lilypond input. Within a Scheme expression, you
can use, define or change lilypond variables. "

}


foo = \notes \transpose c' { d''4-. }
bra = \notes \transpose c' { e'4-. }

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
}
