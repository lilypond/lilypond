\version "1.7.18"
\header {
    texidoc = "Clusters are a device to denote that a complete range of
notes is to be played."
}

fragment =\notes \relative c' {
         c4 f4 a4 <<e d'>>4
         <<g a>>8 <<e a>> a4 c2 <<d b>>4 e4 
         c4 a4 f4 g4 a4 }

\score {
 \notes <
     \context Staff = SA \fragment
     \context Staff = SB
       \context Voice <
          \fragment
          { s2 \hideNotes
            s4-\startCluster
            s1*2
            s4-\stopCluster
	    \unHideNotes
	} >
     >
}


%% new-chords-done %%
