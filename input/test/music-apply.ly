
mus = \notes { c'4 d'4( e'4 f'4  }

#(define (reverse-music mus)
  (let* (
	(es (ly-get-mus-property mus 'elements))
	(reved (reverse es))
	(sd (ly-get-mus-property mus 'span-direction))
	)
	(ly-set-mus-property
		mus
		'elements
		reved
	)
	(if (dir? sd)
		(ly-set-mus-property mus 'span-direction (- sd))) 
	(map reverse-music reved)
	mus)
)

\score {
	\context Voice {
		\mus
		\apply #reverse-music \mus
	}
}
