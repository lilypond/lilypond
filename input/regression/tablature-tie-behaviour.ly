\version "2.19.21"

\header{ texidoc = "In tablature, notes that are tied to are invisible
                     except after a line break or within a second volta;
                    here, the fret number is displayed in parentheses.

                    As an option, the notes that are tied to may become
                    invisible completely, even after line breaks."
        }

firstpart = \relative {
  f2 ~ 4   e
  g8 g ~ g g ~ g g~ g g ~
  1
}

secondpart = \relative {
  c'1 ~ \break c2 ~ 2
}

thirdpart = \relative {
    \repeat volta 2 {
    < c'\3 e\2 g\1 >4 < c\3 e\2 g\1 > ~ < c\3 e\2 g\1 >\laissezVibrer r
    c4. d8 e2 ~
   }
   \alternative { { e2 r } { e2\repeatTie e2^\fermata } }
 }

\context StaffGroup <<
  \context Staff {
     \clef "G_8"
    \hide Voice.StringNumber % remove circled string numbers
    \firstpart
    \secondpart
    \thirdpart
  }
  \context TabStaff {
    \firstpart
    \secondpart
    \thirdpart
  }
>>

\context StaffGroup <<
  \context Staff {
    \clef "G_8"
    \hide Voice.StringNumber % remove circled string numbers
    \firstpart
    \secondpart
    \thirdpart
    \secondpart
    \thirdpart
  }
  \context TabStaff {
    \hideSplitTiedTabNotes
    \firstpart
    \secondpart
    \thirdpart
    \showSplitTiedTabNotes
    \secondpart
    \thirdpart
  }
>>
