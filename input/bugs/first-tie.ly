\header{
    texidoc="no tie is generated for first chord of score.  workaround: begin
with s1*0 spacer note."
    }

\score {
    \context Staff \notes {
	% s1*0
	<a1> ~ <a>
    }
}