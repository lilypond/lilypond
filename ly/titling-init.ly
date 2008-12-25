\version "2.12.0"

slashSeparator = \markup {
  \center-align
  \vcenter \combine
  \beam #2.0 #0.5 #0.48
  \raise #0.7 \beam #2.0 #0.5 #0.48
}

tagline = \markup {
  \with-url

  %% todo: lilypond.org/music-engraving
  #"http://lilypond.org/web/"
  \line {

    %% 2014 = em dash.
    
    #(ly:export
      (format "Music engraving by LilyPond ~a~awww.lilypond.org"
       (lilypond-version)
       (ly:wide-char->utf-8 #x2014)
       ))
  }
}

#(define (print-all-headers layout props arg)
  (if (eq? (ly:output-def-lookup layout 'print-all-headers) #t)
   (interpret-markup layout props arg)
   empty-stencil))

bookTitleMarkup = \markup {
  \override #'(baseline-skip . 3.5)
  \column {
    \fill-line { \fromproperty #'header:dedication }
    \override #'(baseline-skip . 3.5)
    \column {
      \huge \larger \bold
      \fill-line {
        \larger \fromproperty #'header:title
      }
      \fill-line {
        \large \smaller \bold
        \larger \fromproperty #'header:subtitle
      }
      \fill-line {
        \smaller \bold
        \fromproperty #'header:subsubtitle
      }
      \fill-line {
        \fromproperty #'header:poet
        { \large \bold \fromproperty #'header:instrument }
        \fromproperty #'header:composer
      }
      \fill-line {
        \fromproperty #'header:meter
        \fromproperty #'header:arranger
      }
    }
  }
}

scoreTitleMarkup = \markup { \column {
  \on-the-fly #print-all-headers { \bookTitleMarkup \hspace #1 }
  \fill-line {
    \fromproperty #'header:piece
    \fromproperty #'header:opus
  }
}
}

%% Book first page and last page predicates
#(define (first-page layout props arg)
  (define (ancestor layout)
    "Return the topmost layout ancestor"
    (let ((parent (ly:output-def-parent layout)))
       (if (not (ly:output-def? parent))
           layout
           (ancestor parent))))
  (if (= (chain-assoc-get 'page:page-number props -1)
         (ly:output-def-lookup (ancestor layout) 'first-page-number))
      (interpret-markup layout props arg)
      empty-stencil))

#(define (last-page layout props arg)
  (if (and (chain-assoc-get 'page:is-bookpart-last-page props #f)
           (chain-assoc-get 'page:is-last-bookpart props #f))
      (interpret-markup layout props arg)
      empty-stencil))

#(define (not-first-page layout props arg)
  (define (ancestor layout)
    "Return the topmost layout ancestor"
    (let ((parent (ly:output-def-parent layout)))
       (if (not (ly:output-def? parent))
           layout
           (ancestor parent))))
  (if (not (= (chain-assoc-get 'page:page-number props -1)
              (ly:output-def-lookup (ancestor layout) 'first-page-number)))
      (interpret-markup layout props arg)
      empty-stencil))

%% Bookpart first page and last page predicates
#(define (part-first-page layout props arg)
  (if (= (chain-assoc-get 'page:page-number props -1)
         (ly:output-def-lookup layout 'first-page-number))
      (interpret-markup layout props arg)
      empty-stencil))

#(define (part-last-page layout props arg)
  (if (chain-assoc-get 'page:is-bookpart-last-page props #f)
      (interpret-markup layout props arg)
      empty-stencil))

%% unused
#(define (not-single-page layout props arg)
  (if (not (and (= (chain-assoc-get 'page:page-number props -1) 
                   (ly:output-def-lookup layout 'first-page-number))
               (chain-assoc-get 'page:last? props -1)))
   (interpret-markup layout props arg)
   empty-stencil))

#(define (create-page-number-stencil layout props arg)
  (if (eq? (ly:output-def-lookup layout 'print-page-number) #t)
   (interpret-markup layout props arg)
   empty-stencil))

#(define (print-page-number-check-first layout props arg)
  (if (or (not (= (chain-assoc-get 'page:page-number props -1) 
                  (ly:output-def-lookup layout 'first-page-number)))
          (eq? (ly:output-def-lookup layout 'print-first-page-number) #t))
   (create-page-number-stencil layout props arg)
   empty-stencil))

oddHeaderMarkup = \markup
\fill-line {
  %% force the header to take some space, otherwise the
  %% page layout becomes a complete mess.
  " "
  \on-the-fly #not-first-page \fromproperty #'header:instrument
  \on-the-fly #print-page-number-check-first \fromproperty #'page:page-number-string
}

evenHeaderMarkup = \markup
\fill-line {
  \on-the-fly #print-page-number-check-first \fromproperty #'page:page-number-string
  \on-the-fly #not-first-page \fromproperty #'header:instrument
  " "
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

