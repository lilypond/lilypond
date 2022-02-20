\header {
  texidoc = "Clipping snippets from a finished score

Notes:

@itemize @bullet
@item If system starts and ends are included, they include extents of the System grob, eg. instrument names.
@item Grace notes  at the end point of the region are not included
@item Regions can span multiple systems. In this case, multiple EPS files are generated.
@end itemize

This file needs to be run separately with @option{-dclip-systems}; the
collated-files.html of the regression test does not adequately show
the results.

The result will be files named
@file{@var{base}-from-@var{start}-to-@var{end}[-@var{count}].eps}.

When using Cairo, this file only works when using the PostScript format.
"

}

\version "2.21.0"

#(ly:set-option 'clip-systems)
#(define clip-input-suffix "clip-input")
#(define output-suffix clip-input-suffix)

origScore = \score{
    \relative {
      \set Staff.instrumentName = "bla"
      c'1
      d
      \grace c16
      e1
      \key d \major

      f
      \break  \clef bass
      g,
      fis
    }
}

\book { 
  \score {
    \origScore
    \layout {

      %% each clip-region is a (START . END) pair
      %% where both are rhythmic-locations.
      
      %% (make-rhythmic-locations BAR-NUMBER NUM DEN)
      %% means NUM/DEN whole-notes into bar numbered BAR-NUMBER

      clip-regions
      = #(list
	  (cons
	   (make-rhythmic-location 2 0 1)
	   (make-rhythmic-location 4 0 1))

	  (cons
	   (make-rhythmic-location 0 0 1)
	   (make-rhythmic-location 4 0 1))
	  
	  (cons
	   (make-rhythmic-location 0 0 1)
	   (make-rhythmic-location 6 0 1))
	)
    }
  }
}

#(ly:set-option 'clip-systems #f)
#(define output-suffix #f)

clipname = "from-2.0.1-to-4.0.1-clip.eps"

clipMarkup = \markup { \epsfile #X #30.0 #(format #f "~a-~a-~a" (ly:parser-output-name) clip-input-suffix clipname) }

\book {
  \score { \origScore }
  \markup { \bold \fontsize #6 clips }
  \score {
    \lyrics {
      \markup { \clipname }
      \clipMarkup
    }
  }
}
