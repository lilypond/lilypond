\version "2.16.0"
#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (ly:translate-cpp-warning-scheme "adding note head to incompatible stem (type = %d/%d)") 1 1)
#(ly:expect-warning (ly:translate-cpp-warning-scheme "maybe input should specify polyphonic voices"))
#(ly:expect-warning (ly:translate-cpp-warning-scheme "adding note head to incompatible stem (type = %d/%d)") 1 8)
#(ly:expect-warning (ly:translate-cpp-warning-scheme "maybe input should specify polyphonic voices"))
#(ly:expect-warning (ly:translate-cpp-warning-scheme "adding note head to incompatible stem (type = %d/%d)") 4 1)
#(ly:expect-warning (ly:translate-cpp-warning-scheme "maybe input should specify polyphonic voices"))
#(ly:expect-warning (ly:translate-cpp-warning-scheme "adding note head to incompatible stem (type = %d/%d)") 1 1)
#(ly:expect-warning (ly:translate-cpp-warning-scheme "maybe input should specify polyphonic voices"))
#(ly:expect-warning (ly:translate-cpp-warning-scheme "adding note head to incompatible stem (type = %d/%d)") 1 4)
#(ly:expect-warning (ly:translate-cpp-warning-scheme "maybe input should specify polyphonic voices"))
#(ly:expect-warning (ly:translate-cpp-warning-scheme "adding note head to incompatible stem (type = %d/%d)") 1 1)
#(ly:expect-warning (ly:translate-cpp-warning-scheme "maybe input should specify polyphonic voices"))
#(ly:expect-warning (ly:translate-cpp-warning-scheme "adding note head to incompatible stem (type = %d/%d)") 1 4)
#(ly:expect-warning (ly:translate-cpp-warning-scheme "maybe input should specify polyphonic voices"))
#(ly:expect-warning (ly:translate-cpp-warning-scheme "adding note head to incompatible stem (type = %d/%d)") 1 8)
#(ly:expect-warning (ly:translate-cpp-warning-scheme "maybe input should specify polyphonic voices"))


\header {

  texidoc = "
Combine several kinds of stems in parallel voices.
"

}

\new Voice { \time 4/1
	     << c'\breve e'8 >>
	     << c'8 e'\breve >> |
	     << c'\longa e'1 >> |
	     << c'1 e'\longa >> |
	     << c'2 e'1 >>
	     << c'1 e'2 >>
	     << c'2 e'4 >>
	     << c'4 e'2 >>
	     << c'2 e'8 >>
	     << c'8 e'2 >>
}
