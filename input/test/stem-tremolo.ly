
\version "1.3.5";
\score{
	\context Voice\notes \relative c''{
		%%?
		\property Voice.textVerticalDirection = #1
		%% burp, is this in staff or half spaces, or what?
		\property Voice.textScriptPadding = #15
		a1:4^":4" a:8^":8" c:16^":16" a:32^":32" a^"x" a:^":"
		a4:4 c:8 a:16 c:32 a a: a2:
		\break
		\stemup
		a4:32 a'4:64 
		\stemdown
		c,4:32 c,4:64
		\stemboth
		c'8:16 c c c
		a': a a: a
		c,16:32 c: c c a16:32 a: a a
		c8:16 g: d: a:
		c8:32 f: b: e:
	}
}
