%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\version "2.5.18"

%% demonstration of line arrows
%% By Jonatan Liljedahl <http://kymatica.com>

\relative c'' {
    %% Arrow between two texts    
    \override TextSpanner #'edge-text = #'("foo" . "bar")
    \override TextSpanner #'dash-fraction = #'()
    \override TextSpanner #'style = #'line
    \override TextSpanner #'arrow = ##t
    
    %% Default arrow size
%    \override TextSpanner #'arrow-length = #1.3
%    \override TextSpanner #'arrow-width = #0.5

    %% We can put arrows to a glissando line
    \override Glissando #'arrow = ##t
    
    a8\startTextSpan gis8\< a2 b4\glissando
    c,4 g'\! c\stopTextSpan c
}
