\version  "1.9.8"

\header {
texidoc ="Hairpin crescendi may be dashed. "

}

\score {
    \notes \relative c' {
	\property Voice.Hairpin \set #'dash-fraction = #0.4
	\property Voice.Hairpin \set #'dash-period = #1
	f2\< g c1 d4\> b a gis\! }     
    }
