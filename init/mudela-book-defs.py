# Default mudela-book init-file
{    'mudela': r"""
\begin{mudela}[eps \fontoptions]
    \maininput
\end{mudela}
""",
    'mudelaRhythm': r"""
\begin{mudela}[eps \fontoptions]
\score{
    \type RhythmicStaff{
        \notes{\stemup \maininput}
    }
    \paper{linewidth = -1.\cm;}
}
\end{mudela}
"""
}
