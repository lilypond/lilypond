
\header {
texidoc = "@cindex Ancient Accidentals
Accidentals are available in different ancient styles, which all
are collected here.
"
}

\version "2.1.30"

\score { 
  \context Voice \notes\relative c {
    \time 3/4
	\override Staff.Accidental  #'style = #'default
	cisis''^"Accidental style = \#'default" cisis! cisis? |
	cis cis! cis? | 
	c c! c? |
	ces ces! ces? |
	ceses ceses! ceses? |
	\break
	
	\override Staff.Accidental  #'style = #'hufnagel
	cisis^"Accidental style = \#'hufnagel" cisis! cisis? |
	cis cis! cis? | 
	c c! c? |
	ces ces! ces? |
	ceses ceses! ceses? |
	\break
	
	\override Staff.Accidental  #'style = #'medicaea
	cisis^"Accidental style = \#'medicaea" cisis! cisis? |
	cis cis! cis? | 
	c c! c? |
	ces ces! ces? |
	ceses ceses! ceses? |
	\break
	
	\override Staff.Accidental  #'style = #'vaticana
	cisis^"Accidental style = \#'vaticana" cisis! cisis? |
	cis cis! cis? | 
	c c! c? |
	ces ces! ces? |
	ceses ceses! ceses? |
	\break
	
	\override Staff.Accidental  #'style = #'mensural
	cisis^"Accidental style = \#'mensural" cisis! cisis? |
	cis cis! cis? | 
	c c! c? |
	ces ces! ces? |
	ceses ceses! ceses? |
	
  }
}

