\version "1.3.117";

somewhatLong =  \lyrics{
	\property Lyrics . textStyle = "roman" 
	AaaaaaA2
	BbbbbbB2
	CcccccC2 
	DdddddD2
	EeeeeeE2
	FfffffF2
}

ratherLong =  \lyrics{
	\property Lyrics . textStyle = "roman" 
	LLLLoooonggggg2 
	Syyllllaaabbble2 
	LLLLoooonggggg2 
	Syyllllllaaabbble2
	LLLLoooonggggg2 
	Syyyylllaaabbble2
}

quiteLong =  \lyrics{
	\property Lyrics . textStyle = "roman"
	LLLLLLLLLooooongggggggg2 
	Syyyyyyyyyyyyylllllllaaaaaabbble2 
	LLLLLLLLLooooongggggggg2 
	Syyyyyyyyyyyyylllllllaaaaaabbble2
	LLLLLLLLLooooongggggggg2 
	Syyyyyyyyyyyyylllllllaaaaaabbble2
}

somewhatLongLyricsStaff =  \context Lyrics = somewhat <
	\somewhatLong
>

ratherLongLyricsStaff =  \context Lyrics = rather <
	\ratherLong
>

quiteLongLyricsStaff =  \context Lyrics = quite <
	\quiteLong
>

melody =  \notes
\transpose c'{
	c4 e4 g4 e4  c4 e4 g4 e4  c4 e4 g4 e4  c4 e4 g4 e4
	c4 e4 g4 e4  c4 e4 g4 e4  c4 e4 g4 e4  c4 e4 g4 e4
}

melodicStaff =  \context Staff = mel <
	\melody
>

\score
{
	<
		\melodicStaff
		\somewhatLongLyricsStaff	
%		\ratherLongLyricsStaff	
		\quiteLongLyricsStaff	
	>
	\paper{

	}
}
