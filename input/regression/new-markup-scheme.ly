
\header {

    texidoc = "There is a Scheme macro @code{markup} to produce markup
    texts using a similar syntax as @code{\\markup}."

    }
\version "2.3.2"
\score {
    \notes {
        \fatText
        f'1-\markup {
            foo
            \raise #0.2 \hbracket \bold bar
            \override #'(baseline-skip . 4)

            \bracket \column < baz bazr bla >
            \hspace #2.0
            \override #'(font-family . music) {
                \lookup #"noteheads-0"
                \char #53
            }
            \musicglyph #"accidentals--1"
            \combine "X" "+"   
            \combine "o" "/"
            \box \column < { "string 1" } { "string 2" } >
            "$\\emptyset$"
            \italic Norsk
            \super "2"
            \dynamic sfzp
            \huge { "A" \smaller "A" \smaller \smaller "A"
                    \smaller \smaller \smaller "A" }
            \sub "alike"
        }	
        \break
        f'1-#(markup* 
              "foo"
              #:raise 0.2 #:hbracket #:bold "bar"
              #:override '(baseline-skip . 4) 
              #:bracket #:column ( "baz" "bazr" "bla" )
              #:hspace 2.0
              #:override '(font-family . music) #:line (#:lookup "noteheads-0" 
                                                        #:char 53)
              #:musicglyph "accidentals--1"
              #:combine "X" "+"   
              #:combine "o" "/"
              #:box #:column ("string 1" "string 2")
              "$\\emptyset$"
              #:italic "Norsk"
              #:super "2"
              #:dynamic "sfzp"
              #:huge #:line ("A" #:smaller "A" #:smaller #:smaller "A" 
                             #:smaller #:smaller #:smaller "A")
              #:sub "alike")
    }
    \paper { 
        raggedright = ##t
        indent = #0
        \context {
            \Staff
            \remove Time_signature_engraver 
        }
    }
}
