
\version "2.3.16"
% keep for now, although merging into beam-auto-override is a possibility.
\header {
	texidoc = "@cindex Auto Beaming 4/8
You can override the automatic beaming settings.
"
}

\score{
   \relative c''{
     \time 4/8

%{
    the default for 4/8 (see scm/auto-beam.scm)
     ----  --------
     |  |  |   |--|
    x| x| x|  x| x|
%}
     c8 c c c16 c


%{
    user override
     --------------
     |  |  |   |--|
    x| x| x|  x| x|
%}
     #(override-auto-beam-setting '(end * * * *)  2 4)
     c8 c c c16 c

  }
\paper{raggedright = ##t}
}
    

