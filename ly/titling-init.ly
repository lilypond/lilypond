\version "2.4.0"

slashSeparator = \markup {
  \hcenter
  \vcenter \combine
  \beam #2.0 #0.5 #0.48
  \raise #0.7 \beam #2.0 #0.5 #0.48
}

tagline = \markup {
  \with-url

  %% todo: lilypond.org/music-engraving
  #"http://lilypond.org/web/"
  \line {
    "Music engraving by LilyPond"
    #(ly:export (lilypond-version))

    %% 2014 = em dash.
    #(ly:export (ly:wide-char->utf-8 #x2014))
    "www.lilypond.org"
  }
}

bookTitleMarkup = \markup {
  \override #'(baseline-skip . 3)
  \column {
    \fill-line { \fromproperty #'header:dedication }
    \column {
      \override #'(baseline-skip . 3.5)
      \huge \bigger \bold
      \fill-line {
	\bigger \fromproperty #'header:title
      }
      \fill-line {
	\large \smaller \bold
	\bigger  \fromproperty #'header:subtitle
      }
      \fill-line {
	\smaller \bold
	\fromproperty #'header:subsubtitle
      }
      \fill-line {
	\fromproperty #'header:poet
	{ \large \bold \fromproperty #'header:instrument }
	\column {
	  \fromproperty #'header:composer
	  \fromproperty #'header:arranger
	}
      }
    }
  }
}

scoreTitleMarkup = \markup {
  \fill-line {
    \fromproperty #'header:piece
    \fromproperty #'header:opus
  }
}

#(define (first-page layout props arg)
  (if (= (chain-assoc-get 'page:page-number props -1) 1)
   (interpret-markup layout props arg)
   empty-stencil))

#(define (last-page layout props arg)
  (if (chain-assoc-get 'page:last? props #f)
   (interpret-markup layout props arg)
   empty-stencil))

#(define (not-first-page layout props arg)
  (if (not (= (chain-assoc-get 'page:page-number props -1) 1))
   (interpret-markup layout props arg)
   empty-stencil))

#(define (not-single-page layout props arg)
  (if (not (and (= (chain-assoc-get 'page:page-number props -1) 1)
	    (chain-assoc-get 'page:last? props -1)))
   (interpret-markup layout props arg)
   empty-stencil))

oddHeaderMarkup = \markup
 % \on-the-fly #not-single-page
\fill-line {
  %% force the header to take some space, otherwise the
  %% page layout becomes a complete mess. 
  " "
  \on-the-fly #not-first-page \fromproperty #'header:instrument
  \on-the-fly #not-first-page \fromproperty #'page:page-number-string
}

evenHeaderMarkup = \markup
\fill-line {
  \fromproperty #'page:page-number-string
  \fromproperty #'header:instrument
  ""
}

oddFooterMarkup = \markup {
  \column {
    \fill-line {

      %% Copyright header field only on first page.
      \on-the-fly #first-page \fromproperty #'header:copyright
    }
    \fill-line {
      %% Tagline header field only on last page.
      \on-the-fly #last-page \fromproperty #'header:tagline
    }
  }
}


