\score{
\notes\relative c''{
a1\fff\> \!c,,\pp a'' a\p


% We need this to test if we get two Dynamic line spanners
a

% because Dynamic_engraver::do_removal_processing ()
% doesn't seem to do its job?
a\f

}
\paper{
}
\midi{
\tempo 1 = 60;
}
}
