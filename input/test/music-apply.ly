\header {
texidoc="
Simple customised music apply.
";
}
	
music = \notes { c'4 d'4( e'4 f'4 }

#(define (reverse-music music)
  (let* ((elements (ly-get-mus-property music 'elements))
	 (reversed (reverse elements))
	 (span-dir (ly-get-mus-property music 'span-direction)))
    
    (ly-set-mus-property music 'elements reversed)
    
    (if (dir? span-dir)
	(ly-set-mus-property music 'span-direction (- span-dir)))
    
    (map reverse-music reversed)
    
    music))

\score {
  \context Voice {
    \music
    \apply #reverse-music \music
  }
  \paper {
    linewidth = -1.;
  }
}
