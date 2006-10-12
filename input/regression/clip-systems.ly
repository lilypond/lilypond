\header {
  texidoc = "Clipping snippets from a finished score

Notes:

@itemize @bullet
@item If system starts and ends are included, they include extents of the System grob, eg. instrument names.
@item Grace notes  at the end point of the region are not included
@item Regions can span multiple systems. In this case, multiple EPS files are generated.
@end itemize

This file needs to be run separately with @code{-dclip-systems}; the
collated-files.html of the regression test does not adequately show
the results.

The result will be files named
@file{@var{base}-system-@var{systemnumber}-@var{start}-@var{end}.eps}.

"

}

\version "2.9.23"


% each clip-region is a (START . END) pair
% where both are rhythmic-locations.

% (make-rhythmic-locations BAR-NUMBER NUM DEN)
% means NUM/DEN whole-notes into bar numbered BAR-NUMBER

\paper {

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

\relative {
  \set Staff.instrumentName = #"bla"
  c1
  d
  \grace c16
  e1
  \key d\major
  
  f
  \break  \clef bass
  g,
  fis
}  
