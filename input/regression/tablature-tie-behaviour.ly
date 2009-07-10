\version "2.13.4"

\header{ texidoc = "In tablature, notes that are tied to are invisible
                    except after a line break or within a second volta;
                    here, the fret number is displayed in parentheses."
       }

tietest = \relative c {
  \override Voice.StringNumber #'transparent = ##t % remove circled string numbers
  \repeat volta 2 {
    f2 ~ f4  e
    g8 g ~ g g ~ g g~ g g ~
    g1
    c1 ~ \break  c2 ~ c
    < c\3 e\2 g\1 >4 < c\3 e\2 g\1 > ~ < c\3 e\2 g\1 >\laissezVibrer r
    c4. d8 e2 ~
  }
  \alternative { { e2 r } { e2\repeatTie e2^\fermata } }
  \bar "|."
}

\context StaffGroup <<
  \context Staff <<
    \clef "G_8"
    \tietest
  >>
  \context TabStaff <<
    \tietest
  >>
>>

