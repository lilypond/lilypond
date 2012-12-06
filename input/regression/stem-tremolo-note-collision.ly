\version "2.17.2"

\header {
  texidoc = "Tremolos should avoid other notes in the staff as
best as possible and issue a warning otherwise.
"
}

#(ly:expect-warning (_ "ignoring too many clashing note columns"))

{
<<
   {  b'4 f'2. }
   \\
   {
     \grace a8
     \repeat tremolo 32 <b g'>32
   }
>>
}