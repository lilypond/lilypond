\version "1.3.80";
% bagpipe music.

\header
{
 title="Flower of Scotland";
}

\paper {
  linewidth = 14.0 \cm;
  indent = 0.0 \cm;
  \translator {
  	\GraceContext

	basicNoteHeadProperties \push #'font-size = #-2
	basicNoteHeadProperties \push #'font-size = #-2	
        basicStemProperties \push #'flag-style = ##f
    % The following determines the length of stems without beams
    % default is between 2.8 and 4.0 depending on the number of flags
         basicStemProperties \push #'length = #6	
        basicStemProperties \push #'font-size = #-2
	basicGraceAlignItemProperties \push #'horizontal-space = #1
	basicStemProperties \push #'flag-style = #""
 
  }
  \translator { \StaffContext
  	basicTimeSignatureProperties \push #'style = #"C4/4"
  	basicTimeSignatureProperties \push #'visibility-function = #begin-of-line-visible
  }	
}


\notenames #'(
	(g . ( 0 4 0 ))
        (a . ( 0 5 0 ))
        (b . ( 0 6 0 ))
        (c . ( 1 0 0 ))
        (d . ( 1 1 0 ))
        (e . ( 1 2 0 ))
        (f . ( 1 3 0 ))
)



taor = \notes{ \grace { [g32 d g e]}}
grip = \notes{ \grace { [g32 b g ]}}
thrd = \notes{ \grace { [g32 d c] }}
birl = \notes{ \grace { [g32 a g] }}
gstd = \notes{ \grace { [g'32 d g] }}
 lgg = \notes{ \grace { g32 }}
 lag = \notes{ \grace { a32 }}
 fgg = \notes{ \grace { [f32 g'32] }}
dblb = \notes{ \grace { [g'32 b d] }}
dblc = \notes{ \grace { [g'32 c d] }}
dble = \notes{ \grace { [g'32 e f] }}
dblf = \notes{ \grace { [g'32 f g'] }}
dblg = \notes{ \grace { [g'32 f] }}
dbla = \notes{ \grace { [a'32 g'] }}
cg   = \notes{ \grace { c32 }}
eg   = \notes{ \grace { e32 }}
gg   = \notes{ \grace { g'32 }}
dg   = \notes{ \grace { d32 }}
hag  = \notes{ \grace { a'32 }}
gefg = \notes{ \grace { [g'32 e f] }}
efg  = \notes{ \grace { [e32 f] }}
gdcg = \notes{ \grace { [g'32 d c]}}
gcdg = \notes{ \grace { [g'32 c d]}}



% 
\score {
  \notes { \time 6/8; \partial 4;
  \property Voice.tieVerticalDirection = \up
  \slurup
f4 |
\gg f4 e8 \thrd d4. |
\eg a4.()a4 d8 |
\gg d4 f8 \dble e4. ( | \nobreak
)e8 d4 \gg d4 e8 |

\break
\time 9/8;
\dblf f2.( )f4 d8 |
\time 6/8;
\dblg g'4 a'8 \gg a'4. |
\thrd d4.( )d4 \eg a8 |
\time 9/8;
\dble e4 \lag e8 \gg [e16 d8. e8] \gg f4 g'8 |

\break
\time 6/8;
\gg f4 e8 \thrd d4. |
\eg a4.( )a4 d8 |
\dblg g'4 a'8 \gg a'4. |
\thrd d4.( )d4 f8 |

\break
\dblg g'4 e4 \dblf [f8. e16] |
\thrd d4.( )d4 \cg d8 |
\gg c4 e8 \thrd d4.( |
)d4. \gdcg d4.
  }
}
