\version "1.7.22"
% TODO: check about other ottava functions.

\header { texidoc = "@cindex Ottava Spanner
You can make Lilypond print ottava spanners. "
}

\score{
    \notes\relative c''{
        \property Voice.TextSpanner \override #'edge-text = #'("8 " . "")
        a #(ly:export (make-span-event 'TextSpanEvent START))
	b c 
        a #(ly:export (make-span-event 'TextSpanEvent STOP))

        \property Voice.TextSpanner \revert #'style
        \property Voice.TextSpanner \override #'style = #'dotted-line
        a #(ly:export (make-span-event 'TextSpanEvent START))
	b c 
        a #(ly:export (make-span-event 'TextSpanEvent STOP))

        \property Voice.TextSpanner \revert #'style
        \property Voice.TextSpanner \override #'style = #'dashed-line
        \property Voice.TextSpanner \override #'edge-height = #'(1 . -2)
        a #(ly:export (make-span-event 'TextSpanEvent START))
	b c 
        a #(ly:export (make-span-event 'TextSpanEvent STOP))


        \property Staff.centralCPosition = #-13

        \property Voice.TextSpanner \override #'dash-length = #5
        \property Voice.TextSpanner \override #'thickness = #10
        a #(ly:export (make-span-event 'TextSpanEvent START))
	b c 
        a #(ly:export (make-span-event 'TextSpanEvent STOP))
        \property Staff.centralCPosition = #-6	
    }
	\paper{ raggedright = ##t}
}
%% new-chords-done %%
