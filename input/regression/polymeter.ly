\version "2.25.30"

\header {
  texidoc = "The @code{\enablePerStaffTiming} command makes @code{measureLength}
and other timing properties independent between staves."
}

#(ly:set-option 'warning-as-error #t)

\layout {
  \enablePerStaffTiming
}

<<
 \new Staff { \time 4/4 c'1 | d' | f' }
  \new PianoStaff <<
    \new Staff { \time 3/4 c'2. | 4 2 | 2 4 | 4 4 4 }
    \new Dynamics { \time 3/4 s2.\p\< s\> s\! s }
    \new Staff { \time 3/4 c'2. | 4 2 | 2 4 | 4 4 4 }
  >>
 {
   \skip 4 % check after all the \time commands are done
   \contextPropertyCheck Score.beamExceptions \default
   \contextPropertyCheck Score.beatBase \default
   \contextPropertyCheck Score.beatStructure \default
   \contextPropertyCheck Score.measureLength \default
   %% Score.timeSignature 4/4 is initialized in engraver-init.ly, so removing
   %% Timing_translator does not affect it.  It looks inconsistent here, but it
   %% is practical: the Score setting is visible as the default time signature
   %% in each Staff context.
   \contextPropertyCheck Score.timeSignature 4/4
 }
>>
