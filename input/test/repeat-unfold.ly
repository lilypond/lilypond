
\score { \notes\context Staff\relative c'' {
\repeat unfold 3 { c^"3$\\times$ 0alt" d }
% less alts than body
\repeat unfold 4 { c^"4$\\times$ 2alt" d } \alternative { e f }
% more alts than body
\repeat unfold 2 { c^"2$\\times$ 3alt" d } \alternative { e f g } 
}}

