
globalNoKey=\notes {
\time 3/4;
\skip 2.*31;
s2 s8 \bar "||"; \break
s8 
\time 2/4;
\skip 2*225;
\bar "|.";
}

global=\notes {
\key f;
\globalNoKey
}

marks=\notes {
\time 3/4;
\property Voice.textstyle = "Large"
\skip 2.*31;
s2 s8 s^"\\Allegromoltovivace"
\time 2/4;
\skip 2*12;
\mark "A";
\skip 2*12;
\mark "B";
\skip 2*26;
\mark "C";
\skip 2*24;
\mark "D";
\skip 2*32;
\mark "E";
\skip 2*10;
\mark "F";
\skip 2*26;
\mark "G";
\skip 2*16;
\mark "H";
\skip 2*20;
\mark "I";
\skip 2*12;
\mark "K";
\skip 2*16;
\mark "L";
\skip 2*8;
\mark "M";
\skip 2*11;
%slut
}
