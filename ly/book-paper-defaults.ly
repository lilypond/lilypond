\version "2.3.2"

\bookpaper {
    unit = #(ly:unit)
    mm = 1.0
    in = 25.4
    pt = #(/  in 72.27)
    cm = #(* 10 mm)

    inputencoding = #"TeX"
    
   %
   % 20pt staff, 5 pt = 1.75 mm
   %

   outputscale = #1.7573
    
    #(define-public score-title default-score-title)
    #(define-public user-title default-user-title)
    #(define-public book-title default-book-title)

    %
    % ugh. hard coded?
    %
    
    #(paper-set-staff-size (* 20.0 pt))


  papersize = "a4"

    #(define font-defaults
      '((font-encoding . fetaMusic)))

    #(define text-font-defaults
      '((font-encoding . latin1)
	(baseline-skip . 2)
	(word-space . 0.6)))

    #(define page-breaking ly:ragged-page-breaks)
    %%#(define page-breaking ly:optimal-page-breaks)

    #(define page-to-stencil ly:page-header-lines-footer-stencil)

    #(define make-header plain-header)
    #(define make-footer plain-footer)
    %%#(define make-footer empty-markup)
    #(define make-tagline TAGLINE-or-tagline-from-header)
    #(define make-copyright copyright-from-header)
   
}
