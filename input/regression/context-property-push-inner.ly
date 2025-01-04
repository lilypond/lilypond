\version "2.25.24"

\header {
  texidoc = "@code{\\pushContextProperty} avoids accessing the property stacks
of contexts enclosing the target context.  This test should run with expected
warnings and no instrument names should appear in the output."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (G_ "cannot pop from empty stack"))

\new PianoStaff <<
  \new Staff = "RH" {
    \set PianoStaff.instrumentName = "A"
    \pushContextProperty PianoStaff.instrumentName "B"
    \contextPropertyCheck PianoStaff.instrumentName "B"
    %% Now, the stack for PianoStaff.instrumentName holds ("A").

    \set Staff.instrumentName = #'() % which happens to be the default
    \pushContextProperty Staff.instrumentName "C"
    \contextPropertyCheck Staff.instrumentName "C"
    %% Now, the stack for Staff.instrumentName holds (()).  If the push function
    %% improperly read the PianoStaff stack, the Staff stack would instead hold
    %% (() "A").

    \popContextProperty PianoStaff.instrumentName
    \contextPropertyCheck PianoStaff.instrumentName "A"
    %% Now, the stack for PianoStaff.instrumentName is empty.
    \unset PianoStaff.instrumentName

    \popContextProperty Staff.instrumentName
    \contextPropertyCheck Staff.instrumentName #'()
    %% Now, the stack for Staff.instrumentName is empty.

    %% This should warn and unset Staff.instrumentName because the stack is
    %% empty.  In the case of the mistake described above, this would work and
    %% would set Staff.instrumentName to "A".
    \popContextProperty Staff.instrumentName
    \contextPropertyCheck Staff.instrumentName \default
    s1
  }
  \new Staff = "LH" {
    s1
  }
>>
