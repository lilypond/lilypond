#(ly:set-option 'old-relative)
\version "1.9.0"
\header{
texidoc =  "Concurrent tuplets should be spaced equidistantly on
all staffs.

Note that this only spaces correctly (exactly) when raggedright
is. For non-raggedright, it still shows a bug: uneven spacing. 
"  }



multipart =  \notes \relative c'{ 
    \context StaffGroup < 
                \context Staff = ten  \context Voice { 
                   \times 2/10 {  c8-[ c c c c c c c c c] } 
                   \times 2/10 {  c-[  c c c c c c c c c] } 
		}
                \context Staff = eleven  \context Voice { 
                   \times 2/11 {  c8-[ c c c c c c c c c c] } 
                   \times 2/11 {  c-[  c c c c c c c c c c] } 
                }
            >
	}
    
\score{
    \notes { 
	\multipart 
    }

%    \paper { raggedright = ##t }

}


