\header
{
  texidoc  ="Tuplet bracket ends properly when quoting."
}

\version "2.21.0"

\paper { ragged-right = ##t }

\addQuote x {
   \tuplet 3/2 { a'8 a' a' } a'4 a'2 |
}

\new Staff <<
   \set Staff.quotedEventTypes = #'(note-event tuplet-span-event)
   \new Voice = "cue" { s1 }
   \new Voice {
       \cueDuring "x" #1 { r4 }
       c'4
       \cueDuring "x" #1 { r2 }
   }
>>
