\version "1.7.18"
% this looks nice, but I'm not certain that it belongs in input/test
% update: Han-Wen suggests we delete it, since we'll know pretty
% quickly if beams stop working.  :)
\header{
filename = 	 "beams.ly"
composer = 	 "heu"
enteredby = 	 "jcn"
copyright = 	 "PD"

TestedFeatures = 	 "beams and beamflags"
texidoc="DELETE ME
"
}


\score{
	<
		\context GrandStaff < 
\context Staff = SA		\notes\transpose c c { 

			\time 8/4
			\stemUp  c8-[ c'' a' f']
			\stemUp  c16-[ c'' a' f']
			\stemUp  c32-[ c'' a' f']
			\stemUp  c64-[ c'' a' f']
			\stemUp  c128-[ c'' a' f']
			r32

			\stemUp  g8-[ g g g]
			\stemUp  g16-[ g g g]
			\stemUp  g32-[ g g g]
			\stemUp  g64-[ g g g]
			\stemUp  g128-[ g g g]
			r32

						\transpose c' c{
			\stemBoth
			 c'8-[ c']  b-[ b]  a-[ a]  g-[ g]  f-[ f]  e-[ e]
			 c'16-[ c']  b-[ b]  a-[ a]  g-[ g]   f-[ f]  e-[ e]
			 c'32-[ c']  b-[ b]  a-[ a]  g-[ g]  f-[ f]  e-[ e]
			 c'64-[ c']  e-[ e]}
		}
	>
	<	
\context Staff = SB		\notes { 
\transpose c c {
			\time 8/4
			\stemDown  a'8-[ a, c e]
			\stemDown  a'16-[ a, c e]
			\stemDown  a'32-[ a, c e]
			\stemDown  a'64-[ a, c e]
			\stemDown  a'128-[ a, c e]
			r32}

			\transpose c' c{
			\stemDown  d''8-[ d'' d'' d'']
			\stemDown  d''16-[ d'' d'' d'']
			\stemDown  d''32-[ d'' d'' d'']
			\stemDown  d''64-[ d'' d'' d'']
			\stemDown  d''128-[ d'' d'' d'']
			r32}

			\transpose c c'{
			\stemBoth
			 a8-[ a]  b-[ b]  c'-[ c']  d'-[ d']  e'-[ e']  f'-[ f']
			 a16-[ a]  b-[ b]  c'-[ c']  d'-[ d']   e'-[ e']  f'-[ f']
			 a32-[ a]  b-[ b]  c'-[ c']  d'-[ d']   e'-[ e']  f'-[ f']
			 a64-[ a]  f'-[ f']} 
		}
	>
	>

	\paper{

	}
}

%% new-chords-done %%
