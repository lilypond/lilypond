\version "2.27.0"

\header {
  texidoc = "Test for stanza reminders.

When @code{stanzaReminders} is set to @code{#t}, a reminder for the current
stanza number is printed at the beginning of each system.  The reminder
can be adjusted to a specific value or to a procedure that generates it
automatically from the current stanza markup."
}

<<
  \new Staff {
    \repeat unfold 7 { \repeat unfold 8 c'4 \break }
  }
  \new Lyrics \lyricmode {
  \set stanzaReminders = ##t
  \stanza "1."
  \repeat unfold 14 one 4
  \stanza "2."
  \set stanzaReminderText = #make-bracket-markup
  \repeat unfold 14 two 4
  \stanza "3."
  \after 1*2 \noStanza
  \repeat unfold 14 three 4
  \stanza "4."
  \set stanzaReminderText = "(fourth)"
  \repeat unfold 14 four 4
  }
>>
