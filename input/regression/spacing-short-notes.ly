\header {
    
texidoc = "Notes that are shorter than the common shortest note, Get a
space (i.e. without the space needed for the note) proportional to
their duration. So 16th notes get 1/2 of the space of an eigth note.
The total distance for a 16th is (including note head) is 3/4 of the
eighth note. "

}

\score { \notes \relative c''
{
    \time 2/4 
    c16 c c c c4 c4
    c8 c8 c8 c8
    c8 c8 c4
    c8 c8 c4

}

	 \paper { linewidth = -1. }
}
