\version "1.3.4";

$somewhat_long = \lyrics{
	\property Lyrics . textStyle = "roman" 
	AaaaaaA2
	BbbbbbB2
	CcccccC2 
	DdddddD2
	EeeeeeE2
	FfffffF2
}

$rather_long = \lyrics{
	\property Lyrics . textStyle = "roman" 
	LLLLoooonggggg2 
	Syyllllaaabbble2 
	LLLLoooonggggg2 
	Syyllllllaaabbble2
	LLLLoooonggggg2 
	Syyyylllaaabbble2
}

$quite_long = \lyrics{
	\property Lyrics . textStyle = "roman"
	LLLLLLLLLooooongggggggg2 
	Syyyyyyyyyyyyylllllllaaaaaabbble2 
	LLLLLLLLLooooongggggggg2 
	Syyyyyyyyyyyyylllllllaaaaaabbble2
	LLLLLLLLLooooongggggggg2 
	Syyyyyyyyyyyyylllllllaaaaaabbble2
}

$somewhat_long_lyrics_staff = \context Lyrics = somewhat <
	\$somewhat_long
>

$rather_long_lyrics_staff = \context Lyrics = rather <
	\$rather_long
>

$quite_long_lyrics_staff = \context Lyrics = quite <
	\$quite_long
>

melody = \notes
\transpose c'{
	c4 e4 g4 e4  c4 e4 g4 e4  c4 e4 g4 e4  c4 e4 g4 e4
	c4 e4 g4 e4  c4 e4 g4 e4  c4 e4 g4 e4  c4 e4 g4 e4
}

$melodic_staff = \context Staff = mel <
	\melody
>

\score
{
	<
		\$melodic_staff
		\$somewhat_long_lyrics_staff	
%		\$rather_long_lyrics_staff	
		\$quite_long_lyrics_staff	
	>
	\paper{
%		castingalgorithm = \Wordwrap;	
	}
}
