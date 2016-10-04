\version "2.17.6"

\header {
  texidoc = "Shows the output-attributes property of a grob being set.
This should have no effect in the Postscript backend.  In the SVG
backend these settings should produce this group tag:
@code{<g id=\"123\" class=\"foo\" data-whatever=\"bar\"> @dots{} </g>}
"
}

{
  \override NoteHead.output-attributes =
  #'((id . 123) (class . foo) (data-whatever . "bar"))
  c
}
