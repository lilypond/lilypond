\version "1.5.68"

\header {
	crossRefNumber = "1"
	tagline = "Lily was here 1.3.98 -- automatically converted from ABC"
	title = "Scales"
}
voicedefault = \notes {
\property Score.defaultBarType="empty"

\key c \major     c'8 -"major"   d'8    e'8    f'8    g'8    a'8    b'8    
c''8    \key c \ionian     c'8 -"ionian"   d'8    e'8    f'8    g'8    a'8    
b'8    c''8    \key d \dorian     c'8 -"dorian"   d'8    e'8    f'8    g'8    
a'8    b'8    c''8    \key e \phrygian     c'8 -"phrygian"   d'8    e'8    f'8 
   g'8    a'8    b'8    c''8    \key f \lydian     c'8 -"lydian"   d'8    e'8  
  f'8    g'8    a'8    b'8    c''8    \key g \mixolydian     c'8 -"mixolydian" 
  d'8    e'8    f'8    g'8    a'8    b'8    c''8    \key a \minor     c'8 
-"minor"   d'8    e'8    f'8    g'8    a'8    b'8    c''8    \key a \aeolian   
  c'8 -"aeolian"   d'8    e'8    f'8    g'8    a'8    b'8    c''8    
\key b \locrian     c'8 -"locrian"   d'8    e'8    f'8    g'8    a'8    b'8    
c''8    \key f \major     c'8 -"major"   d'8    e'8    f'8    g'8    a'8    
bes'8    c''8    \key f \ionian     c'8 -"ionian"   d'8    e'8    f'8    g'8   
 a'8    bes'8    c''8    \key g \dorian     c'8 -"dorian"   d'8    e'8    f'8  
  g'8    a'8    bes'8    c''8    \key a \phrygian     c'8 -"phrygian"   d'8    
e'8    f'8    g'8    a'8    bes'8    c''8    \key bes \lydian     c'8 
-"lydian"   d'8    e'8    f'8    g'8    a'8    bes'8    c''8    
\key c \mixolydian     c'8 -"mixolydian"   d'8    e'8    f'8    g'8    a'8    
bes'8    c''8    \key d \aeolian     c'8 -"aeolian"   d'8    e'8    f'8    g'8 
   a'8    bes'8    c''8    \key d \minor     c'8 -"minor"   d'8    e'8    f'8  
  g'8    a'8    bes'8    c''8    \key e \locrian     c'8 -"locrian"   d'8    
e'8    f'8    g'8    a'8    bes'8    c''8    
}\score{
        \notes <

	\context Staff="default"
	{
	    \voicedefault 
	}

    >
	\paper {
	}
	\midi {}
}
