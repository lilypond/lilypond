\version "1.7.18"


\chordmodifiers #default-chord-modifier-list


whiteTriangleMarkup =#(make-override-markup '(font-family . math) (make-simple-markup "M"))

blackTriangleMarkup = #(make-override-markup '(font-family . math) (make-simple-markup "N"))

ignatzekExceptionMusic =  \notes {
	<<c e gis>>1-\markup { "+" }
	<<c es ges>>-\markup { \super "o" } % should be $\circ$ ?
	<<c es ges bes>>-\markup { \super \combine "o" "/" }
	<<c es ges beses>>-\markup { \super  "o7" }
}

ignatzekExceptions = #(sequential-music-to-chord-exceptions ignatzekExceptionMusic)

