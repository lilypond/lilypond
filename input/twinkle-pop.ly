\header{
filename =	 "twinkle.ly";
title =	 "Ah, vous dirais-je, maman ";
description =	 "twinkle twinkle in pop-song-settings";
composer =	 "traditional";

enteredby =	 "HWN, chords by Johan Vromans";
copyright =	 "public domain";
}

%{
Tested Features		lyrics and chords
%}

\version "0.1.9";

melodie = \melodic {
        \clef"violin";
        \meter 2/4 ;
        \octave  c';

        c4^"C" c |      g     g |       a^"F"  a |      g2^"C"   |
        f4^"F" f |      e^"C" e |       d^"G7" d |      c2^"C"   |
        g4^"G" g |      f^"F" f |       e^"C"  e |      d^"G7" d |
        g^"G"  g |      f^"F" f |       e^"C"  e |      d^"G7" d |
        c4^"C" c |      g     g |       a^"F"  a |      g2^"C"   |
        f4^"F" f |      e^"C" e |       d^"G7" d |      c2^"C"   |
}

text = \lyric{ 
        \textstyle "italic";
        Ah!4 vous dir- ai_- je ma man2
        Ce4 qui cau- se mon tour- ment2
        Pa-4 pa veut que je rai- so- nne
        Comme4 un- e grand- e per- so- nne
        Moi4 je dis que les bon- bons2
        Val-4 ent mieux que la rai- son2
        
}

\score {
        <  \melodic \type Staff \melodie
           \lyric   \type Lyrics \text
        >
        \paper {  }
}
