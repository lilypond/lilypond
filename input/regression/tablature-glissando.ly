\version "2.19.21"

\header{ texidoc = "Glissando lines in tablature have the right slope."
       }

\paper { ragged-right = ##f } % strech the staff to make glissando lines visible

glissandotest = \relative {
   c4\5 \glissando d\5 \glissando e\5 f\5 |
   c4\5 \glissando d\5 \glissando c2\5 |
   c4\5 \glissando c'\4 c\4 \glissando c,\5
  \bar "|."
}

\context StaffGroup <<
  \context Staff <<
    \clef "G_8"
    \glissandotest
  >>
  \context TabStaff <<
    \glissandotest
  >>
>>
