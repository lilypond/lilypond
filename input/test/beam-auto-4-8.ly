
%% \version "2.7.39"

%% keep for now, although merging into beam-auto-override is a possibility.
\header {
	texidoc = "@cindex Auto Beaming 4/8
You can override the automatic beaming settings.
"
}

\layout{ragged-right = ##t}

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
  %% This has now (2.5.21) changed, (end * * * *) no longer
  %% masks the default config entry ('(end * * 4 8) 1 4))
  %% rather than masking by override:
  %% #(override-auto-beam-setting '(end * * * *) 2 4)
  %% revert the config file setting.
  #(revert-auto-beam-setting '(end * * 4 8) 1 4)
  c8 c c c16 c
}
    

