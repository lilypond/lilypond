\version "1.3.146"

\header{
    texidoc="
Text is set with empty horizontal dimensions.  The boolean property
TextScript.no-spacing-rods is used to control the horizontal size of text.
"
}

\score{
    \notes{
	%% \emptyText
	%% short for \property Voice.TextScript \set #'no-spacing-rods = ##t
	c2_"very wide and long text" c | \break
	%% short for \property Voice.TextScript \set #'no-spacing-rods = ##f
	\fatText
	c_"very wide and long text" c
    }
    \paper {
	linewidth  = 3.0\cm
    }
}
