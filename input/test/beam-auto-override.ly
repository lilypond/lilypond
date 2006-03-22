
%% ugh
%% \version "2.8.0"

\header {

    texidoc = "@cindex Auto Beaming Override

The auto-beamer, which can be overridden, will only engrave beams 
that end before encountering of 
@itemize @bullet
@item a rest,
@item
 an other, manually entered beam, or
@item
 a bar line. 
@end itemize

The @code{autoBeaming} can also be turned off.

"

}

%% TODO: check doc string. -hw

\layout{ragged-right = ##t}
\relative c''{
  %% This has now (2.5.21) changed, (end * * * *) no longer
  %% masks the default config entry ('(end * * 2 4) 1 4))
  %% rather than masking by override:
  %% #(override-auto-beam-setting '(end * * * *) 1 2)
  %% revert the config file setting.
  #(revert-auto-beam-setting '(end * * 2 4) 1 4)
  \time 2/4
  
  %% one beam per measure
  c8 c c c
  c16 c c c c c c c
  
  %% from here on consider ending beam every 1/4 note
  #(override-auto-beam-setting '(end * * * *) 1 4)

  c8 c c c
  %% manually override autobeam with weird beaming
  c8  c[ c] c
  c8 c c r
  c8 c c4
  r8 c c c
  %% no autobeaming
  \set autoBeaming = ##f
  c8 c c c
}


