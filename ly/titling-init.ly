\version "2.21.0"

#(cond-expand
  (guile-2
    (use-modules (ice-9 curried-definitions)))
  (else))

slashSeparator = \markup {
  \center-align
  \vcenter \combine
  \beam #2.0 #0.5 #0.48
  \raise #0.7 \beam #2.0 #0.5 #0.48
}

tagline = \markup {
  \pad-to-box #'(0 . 0) #'(0 . 3)
  {  \with-url

    "https://lilypond.org/"
    \line {

      %% 2014 = em dash.

      #(format #f "Music engraving by LilyPond ~a~awww.lilypond.org"
         (lilypond-version)
         (ly:wide-char->utf-8 #x2014)
         )
    }
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
      \fill-line {
        \huge \larger \larger \bold
        \fromproperty #'header:title
      }
      \fill-line {
        \large \bold
        \fromproperty #'header:subtitle
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
  \on-the-fly \print-all-headers { \bookTitleMarkup \hspace #1 }
  \fill-line {
    \fromproperty #'header:piece
    \fromproperty #'header:opus
  }
}
}

%% Book first page and last page predicates
#(define (book-first-page? layout props)
   "Return #t iff the current page number, got from @code{props}, is the
book first one."
   (= (chain-assoc-get 'page:page-number props -1)
      (book-first-page layout props)))

#(define (book-last-page? layout props)
   "Return #t iff the current page number, got from @code{props}, is the
book last one."
   (and (chain-assoc-get 'page:is-bookpart-last-page props #f)
        (chain-assoc-get 'page:is-last-bookpart props #f)))

#(define (first-page layout props arg)
  (if (book-first-page? layout props)
      (interpret-markup layout props arg)
      empty-stencil))

#(define (last-page layout props arg)
  (if (book-last-page? layout props)
      (interpret-markup layout props arg)
      empty-stencil))

#(define (not-first-page layout props arg)
  (if (not (book-first-page? layout props))
      (interpret-markup layout props arg)
      empty-stencil))

#(define ((on-page nmbr) layout props arg)
 (if (= (chain-assoc-get 'page:page-number props -1) nmbr)
   (interpret-markup layout props arg)
   empty-stencil))

%% Bookpart first page and last page predicates
#(define (part-first-page? layout props)
  (= (chain-assoc-get 'page:page-number props -1)
     (ly:output-def-lookup layout 'first-page-number)))

#(define (part-last-page? layout props)
  (chain-assoc-get 'page:is-bookpart-last-page props #f))

#(define (part-first-page layout props arg)
  (if (part-first-page? layout props)
      (interpret-markup layout props arg)
      empty-stencil))

#(define (not-part-first-page layout props arg)
  (if (not (part-first-page? layout props))
      (interpret-markup layout props arg)
      empty-stencil))

#(define (part-last-page layout props arg)
  (if (part-last-page? layout props)
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
  (if (or (not (book-first-page? layout props))
          (eq? (ly:output-def-lookup layout 'print-first-page-number) #t))
   (create-page-number-stencil layout props arg)
   empty-stencil))

oddHeaderMarkup = \markup
\fill-line {
  ""
  \on-the-fly #not-part-first-page \fromproperty #'header:instrument
  \on-the-fly #print-page-number-check-first \fromproperty #'page:page-number-string
}

%% evenHeaderMarkup would inherit the value of
%% oddHeaderMarkup if it were not defined here
evenHeaderMarkup = \markup
\fill-line {
  \on-the-fly #print-page-number-check-first \fromproperty #'page:page-number-string
  \on-the-fly #not-part-first-page \fromproperty #'header:instrument
  ""
}

oddFooterMarkup = \markup {
  \column {
    \fill-line {
      %% Copyright header field only on first page in each bookpart.
      \on-the-fly #part-first-page \fromproperty #'header:copyright
    }
    \fill-line {
      %% Tagline header field only on last page in the book.
      \on-the-fly #last-page \fromproperty #'header:tagline
    }
  }
}

%% As long as evenFooterMarkup is unset, it inherits the value of
%% oddFooterMarkup, as if this were declared here:
% evenFooterMarkup = \oddFooterMarkup
