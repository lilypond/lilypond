
\version "2.12.0"

\layout {
  indent = #0
  ragged-right = ##t
  \context {
    \Score
    
    proportionalNotationDuration = #(ly:make-moment 1 64)
    \override Beam #'breakable = ##t
    autoBeaming = ##f
    tupletFullLength = ##t
    \override BarNumber #'transparent = ##t
    \override BarLine #'transparent = ##t
    \override SpanBar #'transparent = ##t
    \override Beam #'break-overshoot = #'(-0.5 . 1.0)
    \override TextScript #'staff-padding = #6
    \override Glissando #'thickness = #3
    \override SpacingSpanner #'strict-grace-spacing = ##t
    \override TupletBracket #'bracket-visibility = ##t
    \override NoteColumn #'ignore-collision = ##t
  }
}

\paper {
  oddHeaderMarkup = \markup \fill-line { " " }
  evenHeaderMarkup = \markup \fill-line { " " }
  oddFooterMarkup = \markup { 
    \fill-line { 
      \bold \fontsize #3 \on-the-fly #print-page-number-check-first \fromproperty #'page:page-number-string } }
  evenFooterMarkup = \markup { 
    \fill-line { 
      \bold \fontsize #3 \on-the-fly #print-page-number-check-first \fromproperty #'page:page-number-string } }
  printfirst-page-number = ##t
  print-page-number = ##t
  ragged-last-bottom = ##t
}

#(set-global-staff-size 14)


%% definitions.


ppX = #(make-dynamic-script (markup #:combine #:transparent #:dynamic "f" #:line(#:hspace 0 #:dynamic "pp" #:hspace 0)))
pX = #(make-dynamic-script (markup #:combine #:transparent #:dynamic "f" #:line(#:hspace 0 #:dynamic "p" #:hspace 0)))
mpX = #(make-dynamic-script (markup #:combine #:transparent #:dynamic "f" #:line(#:hspace 0 #:dynamic "mp" #:hspace 0)))
fX = #(make-dynamic-script (markup #:combine #:transparent #:dynamic "f" #:line(#:hspace 0 #:dynamic "f" #:hspace 0)))
ffX = #(make-dynamic-script (markup #:combine #:transparent #:dynamic "f" #:line(#:hspace 0 #:dynamic "ff" #:hspace 0)))
sfp = #(make-dynamic-script "sfp")
sfpp = #(make-dynamic-script "sfpp")
sffp = #(make-dynamic-script "sffp")
sffpp = #(make-dynamic-script "sffpp")

beam = #(define-music-function (parser location left right) (number? number?)
	(cond ((and (= left 0) (> right 0))
			#{
				\set stemRightBeamCount = #$right
			#})

			((and (> left 0) (= right 0))
			#{
				\set stemLeftBeamCount = #$left
			#})

			(else
			#{
				\set stemLeftBeamCount = #$left
				\set stemRightBeamCount = #$right
			#})
	)
)

fraction = #(define-music-function (parser location music) (ly:music?)
	#{ \tweak #'text #tuplet-number::calc-fraction-text $music #})

triangle = #(define-music-function (parser location music) (ly:music?)
	#{ \once \set shapeNoteStyles = #'#(do do do do do do do) $music #})

semicircle = #(define-music-function (parser location music) (ly:music?)
	#{ \once \set shapeNoteStyles = #'#(re re re re re re re) $music #})

blackdiamond = #(define-music-function (parser location music) (ly:music?)
	#{ \once \set shapeNoteStyles = #'#(mi mi mi mi mi mi mi) $music #})

tiltedtriangle = #(define-music-function (parser location music) (ly:music?)
	#{ \once \set shapeNoteStyles = #'#(fa fa fa fa fa fa fa) $music #})

square = #(define-music-function (parser location music) (ly:music?)
	#{ \once \set shapeNoteStyles = #'#(la la la la la la la) $music #})

wedge = #(define-music-function (parser location music) (ly:music?)
	#{ \once \set shapeNoteStyles = #'#(ti ti ti ti ti ti ti) $music #})

harmonic = #(define-music-function (parser location music) (ly:music?)
	#{ \once \set shapeNoteStyles = #'#(harmonic harmonic harmonic harmonic harmonic harmonic harmonic) $music #})

cross = #(define-music-function (parser location music) (ly:music?)
	#{ \once \set shapeNoteStyles = #'#(cross cross cross cross cross cross cross) $music #})

white = #(define-music-function (parser location music) (ly:music?)
	#{ \once \override NoteHead #'duration-log = #1 $music #})

