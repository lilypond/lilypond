\version "1.7.21"
\header
{
    
texidoc = "To selectively tweak spanners after the linebreaking
process, Scheme code must be used.  In this simple example, the tie
after the line break is set transparent."


}

\score { \notes {
    \property Voice.Tie \set #'after-line-breaking-callback
    = #(lambda (x)
	(if (eq? (cadr (ly:get-broken-into (ly:get-original x))) x)
	 (ly:set-grob-property! x 'transparent #t)
	))
    
    c'1 ~ \break c'1

  }
	 \paper { raggedright = ##t }
}
