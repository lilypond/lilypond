
foo = \notes\relative c''   { cis4 cis cis! cis? }
\score {

  <\foo 
	\context NoteNames \foo
>
}
