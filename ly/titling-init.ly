\version "2.21.0"

#(use-modules (lily curried-definitions))

#(define (on-first-page layout props)
  "Whether the markup is printed on the first page of the book."
  (= (chain-assoc-get 'page:page-number props -1)
     (book-first-page layout props)))

#(define (on-last-page layout props)
  "Whether the markup is printed on the last page of the book."
  (and (chain-assoc-get 'page:is-bookpart-last-page props #f)
       (chain-assoc-get 'page:is-last-bookpart props #f)))

#(define (on-first-page-of-part layout props)
  "Whether the markup is printed on the first page of the book part."
  (= (chain-assoc-get 'page:page-number props -1)
     (ly:output-def-lookup layout 'first-page-number)))

#(define (on-last-page-of-part layout props)
  "Whether the markup is printed on the last page of the book part."
  (chain-assoc-get 'page:is-bookpart-last-page props #f))

#(define on-page
  (define-scheme-function (number) (index?)
    "Whether the markup is printed on page @var{number}."
    (lambda (layout props)
      (= (chain-assoc-get 'page:page-number props -1) number))))

#(define (single-page layout props)
  "Whether the output is on a single page."
  (and (on-first-page layout props)
       (on-last-page layout props)))

#(define (should-print-page-numbers-global layout props)
  "Whether the @code{print-@/page-@/numbers} setting of the
@code{\\paper} block is true.  This does not necessarily mean that
the page number should be printed on the current page due to the
special case of the first page in a book."
  (eq? #t (ly:output-def-lookup layout 'print-page-number)))

#(define (should-print-page-number layout props)
  "Whether the page number should be printed on this page.  This depends
on the settings @code{print-@/page-@/numbers} and
@code{print-@/first-@/page-@/number} of the @code{\\paper} block."
  (and (eq? #t (ly:output-def-lookup layout 'print-page-number))
       (or (not (on-first-page layout props))
           (eq? #t (ly:output-def-lookup layout 'print-first-page-number)))))

#(define (should-print-all-headers layout props)
  "Whether the @code{print-@/all-@/headers} variable from the
@code{\\paper} or @code{\\layout} block is true."
  (eq? #t (ly:output-def-lookup layout 'print-all-headers)))

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
  \if \should-print-all-headers { \bookTitleMarkup \hspace #1 }
  \fill-line {
    \fromproperty #'header:piece
    \fromproperty #'header:opus
  }
}
}

oddHeaderMarkup = \markup
\fill-line {
  ""
  \unless \on-first-page-of-part \fromproperty #'header:instrument
  \if \should-print-page-number \fromproperty #'page:page-number-string
}

%% evenHeaderMarkup would inherit the value of
%% oddHeaderMarkup if it were not defined here
evenHeaderMarkup = \markup
\fill-line {
  \if \should-print-page-number \fromproperty #'page:page-number-string
  \unless \on-first-page-of-part \fromproperty #'header:instrument
  ""
}

oddFooterMarkup = \markup {
  \column {
    \fill-line {
      %% Copyright header field only on first page in each bookpart.
      \if \on-first-page-of-part \fromproperty #'header:copyright
    }
    \fill-line {
      %% Tagline header field only on last page in the book.
      \if \on-last-page \fromproperty #'header:tagline
    }
  }
}

%% As long as evenFooterMarkup is unset, it inherits the value of
%% oddFooterMarkup, as if this were declared here:
% evenFooterMarkup = \oddFooterMarkup
