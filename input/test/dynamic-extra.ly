\header{
    texidoc = "Additional tricks for dynamics.  Pi`u forte dynamic script"
}

piuf = #'(columns (italic "pi\\`u") " " (dynamic "f"))

\score{
    \notes\relative c''{
	c4-#'(italic "pi\\`u")-\f
	c-#'(columns (italic "pi\\`u") " " (dynamic "f"))
	c-\piuf
	c
	c2\< \! c2
	
	c2\< \! c2
	}
    }