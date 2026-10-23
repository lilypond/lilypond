\version "2.27.5"

\header { texidoc = "Depending on the setting of @code{includeGraceNotes},
  stanza reminders at the start of a line may appear at the first moment or the
  first main moment of the line (if they happen to be different)." }

\layout { ragged-right = ##t }

<<
  \new Voice="main" \fixed c' {
    \grace { g8 a b } c'2 2 2 2 \break
    \grace { g8 a b } c'2 2 1 \break
    \grace { g8 a b } c'4 4 4 4 \break
    \grace { g8^( a b } c'4 \tuplet 3/2 {4 4 4)} 4
  }
  \new Lyrics \with { stanzaReminders = ##t
                      stanzaReminderText = #make-bracket-markup }
  \lyricsto "main"
  { \stanza "1." this is mu -- sic, this is \set includeGraceNotes = ##t fun,
    and this is hard work but re -- war -- ding. }
>>
