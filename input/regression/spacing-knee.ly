\version "1.7.18"
\header {

    texidoc = "For knees, the spacing correction is such that the
stems are put at regular distances. This effect takes into account the
width of the note heads and the thickness of the stem.
"
    }
\score { \notes
{
g'8-[ g'8-]     
 g''8-[ g g'' g''] 

 %\property Voice.Stem \override #'thickness = #10 
 %g''8-[ g g'' g''] 
    }
\paper { raggedright = ##t}
     }

%% new-chords-done %%
