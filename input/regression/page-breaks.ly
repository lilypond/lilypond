\version "2.4.0"

\header {

  texidoc = "Stress optimal page breaking.  This should look
    nice on 4 a6 pages. "

  
  copyright = "Copyright by /me"
  
  title = "Title"
  subtitle = "(and (the) subtitle)"
  subsubtitle = "Sub sub title"
  poet = "Poet"
  composer = "Composer"
  texttranslator = "Text Translator"
  meter = "Meter (huh?)"
  arranger = "Arranger"
  instrument = "Instrument"
  piece = "Piece"
  opus = "opus 0"
}

#(define (header layout scopes page-number last?)
  (stack-stencils Y DOWN 0
   (list
    (let ((props (page-properties layout)))
     (interpret-markup layout props
      (make-line-markup (list (markup #:box #:fill-line ("" ""))))))
    (plain-header layout scopes page-number last?))))

#(define (footer layout scopes page-number last?)
  (stack-stencils Y DOWN 0
   (list
    (plain-footer layout scopes page-number last?)
    (let ((props (page-properties layout)))
     (interpret-markup layout props
      (make-line-markup (list (markup #:box #:fill-line ("" "")))))))))

\paper {
  raggedlastbottom = ##f
  #(define make-header header)
  #(define make-footer footer)
}

#(set-default-paper-size "a6" 'portrait)

\book {    
  \score {
    \new Staff \relative c' {
      %% 19: ideally cramped
      %%\repeat unfold 19 { a b c d \break }
      %% 15: test even distribution
      \repeat unfold 15 { a b c d \break }
    }
  }
}
