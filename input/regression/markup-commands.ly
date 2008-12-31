\header
{

  texidoc = "test various markup commands."

}
\paper { ragged-right = ##T }
\version "2.12.0"

{
  g'_\markup {
    \column {
      \line { 
	foo \magnify #2 foo
	LOWER \lower #3 LOWER
	\large \bold { normal \normal-text normal }
	Small-Caps \smallCaps 	Small-Caps
      }
      
      \override #'(line-width . 50) 
      \override #'(bla . "This is a field containing text. Blah blah
blah.  This is a field containing text. Blah blah blah.  This is a
field containing text. Blah blah blah.  This is a field containing
text. Blah blah blah. This is a field containing text. Blah blah
blah.") 
      \column  {
	justify:
	\justify-field #'bla
	wordwrap:
	\wordwrap-field #'bla
      }

      draw-line: \draw-line #'(5 . 3)
      \underline "underlined"
    }
  }
}
