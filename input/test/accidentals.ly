% \version "1.5.15.rz1"

mel = \notes {
 d4  dis dis d | d dis disis dis | d des disis d | dis deses d dis ~ | dis dis ~ dis dis ~ | \break
 dis dis cis c | c cis cisis cis | c ces cisis c | cis ceses c cis ~ | cis cis ~ cis cis \bar "|."  | \break
}

\score { \notes \context Staff \transpose c''' {
   \key d \major
   \mel
   \property Score.autoReminders = #'cautionary
   < s1^"$\\backslash$property Score.autoReminders = \\#'cautionary" \mel >
   \property Score.autoReminders = #'accidental
   < s1^"$\\backslash$property Score.autoReminders = \\#'accidental" \mel >
   \property Score.autoReminders = ##f
   \property Score.forgetAccidentals = ##t
   < s1^"$\\backslash$property Score.forgetAccidentals = \\#\\#t" \mel >
   \property Score.forgetAccidentals = ##f
   \property Score.noResetKey = ##t
   < s1^"$\\backslash$property Score.noResetKey = \\#\\#t" \mel >
  }
}

