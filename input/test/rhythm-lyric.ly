text = \lyrics { Feel the rhy- thm }

melody = \notes { 
	e4. d8 e4. c8
}

\score{
	<
		\context Voice \notes\relative c'' \melody
		\context Lyrics \rhythm \melody \text
	>
}
