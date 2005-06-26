
\version "2.6.0"
\header{
  texidoc="
Engraving second intervals is tricky.  We used to have problems with seconds 
being too steep, or getting too long stems.  In a file like this, showing
seconds, you'll spot something fishy very quickly.
 
" }
\score{
    \relative c''{
	\stemUp
	 b8[ c]
	 b16[ c]
	 a'[ b]
    }
    \layout{
	raggedright = ##t
    }
}
