
\header {


texidoc= "Double time signatures are not supported
explicitly, but can be faked by overriding formatting routines. "

}
\version "1.7.18"


#(define (brew-double-time-sig grob)
  (define (frac-to-mol font frac)
    (let*
	(
	 (d (fontify-text font (number->string (cdr frac))))
	 (n (fontify-text font (number->string (car frac))))
	 (c (ly:molecule-combine-at-edge d Y UP n 0.0))
	 )
      (ly:molecule-align-to! c Y CENTER)
      c
    ))
  
  (let*
      
    (
     (chain (Font_interface::get_property_alist_chain grob))
     (font (ly:get-font grob chain))
     (f1 '(6 . 4))
     (musfont (ly:get-font grob (cons (list '(font-relative-size . 2) '(font-family . music)) chain)))
     (plus (ly:molecule-translate-axis (ly:find-glyph-by-name musfont "scripts-stopped") 0.1 Y))
     (f2 '(3 . 2))
     (m1 (frac-to-mol font f1))
     (m2 (frac-to-mol font f2))
     )
     
    
    (ly:molecule-combine-at-edge
     (ly:molecule-combine-at-edge m1 X RIGHT plus 0.2)
     X RIGHT m2  0.2)
    
    )
)


\score  { \notes \relative c'
	  {
	   \property Staff.TimeSignature \override #'molecule-callback = #brew-double-time-sig
	   \time 3/2
	   c2 c c 
	   
	   }

	  }
	  
