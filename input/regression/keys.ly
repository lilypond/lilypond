
\version "2.19.21"
\header{
texidoc="
Key signatures may appear on key changes, even without a bar line.
In the case of a line break, the restoration accidentals are printed at 
end of a line.  If @code{createKeyOnClefChange} is set, key signatures
are created also on a clef change.
"
}



\paper {
    ragged-right = ##T
}

\relative
{
    \set Staff.createKeyOnClefChange = ##t  
    \key bes \major c'2
				%    \key c \major %  \minor
    \key es \major %  \minor
    c2
    \break
    \key bes \major % \major
    c2 \clef alto c2   \key d \major \clef treble c2
    \set Staff.keyAlterations = #`((4 . ,FLAT) (6 . ,THREE-Q-SHARP) (2 . ,SEMI-FLAT))
    e2
}

