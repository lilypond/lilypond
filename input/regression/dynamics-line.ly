
\version "2.16.0"
\header{
  texidoc=" Dynamics appear below or above the staff.  If multiple
dynamics are linked with (de)crescendi, they should be on the same
line. Isolated dynamics may be forced up or down.
 "
}



\relative c''{
  a1^\sfz
  a1\fff\> c,,\!\pp a'' a\p

  %% We need this to test if we get two Dynamic line spanners
  a

  %% because do_removal_processing ()
  %% doesn't seem to post_process elements
  d\f

  a

}

