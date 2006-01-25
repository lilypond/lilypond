\version "2.6.0"

\paper {

    %%%% WARNING

    %%% if you  add any new dimensions, don't forget to update
    %%% the dimension-variables variable. see paper.scm
    
    unit = #(ly:unit)
    mm = 1.0
    in = 25.4
    pt = #(/  in 72.27)
    cm = #(* 10 mm)

    %% This is weird; `everyone' uses LATIN1?  How does I select TeX
    %% input encoding in EMACS? -- jcn
    %%%%inputencoding = #"TeX"
    inputencoding = #"latin1"
    printpagenumber = ##t

    %%
    %% 20pt staff, 5 pt = 1.75 mm
    %%

    outputscale = #1.7573
    
    #(define-public book-title (marked-up-title 'bookTitleMarkup))
    #(define-public score-title (marked-up-title 'scoreTitleMarkup))
    #(define-public force-eps-font-include #f)
    
    %%
    %% ugh. hard coded?
    %%

    #(layout-set-staff-size (* 20.0 pt))


    #(define-public score-title-properties
      '((is-title . #t)
	(is-book-title . #f)
	))
    #(define-public book-title-properties
      '((is-title . #t)
	(is-book-title . #t)
	))
    
    %%
    %% this dimension includes the extent of the
    %% staves themselves.
    %%
    betweensystemspace = #(* 20 mm)
    
    
    %%
    %% fixed space between systems.
    %%
    betweensystempadding = #(* 4 mm)

    aftertitlespace = 5 \mm
    beforetitlespace = 10 \mm
    betweentitlespace = 2 \mm


    %%
    %% Small staves are aligned so they come out on the same place on
    %% across different pages.
    %%
    pagetopspace = #(* 12 mm)

    
    raggedbottom = ##f

    %%
    %% looks best for shorter scores.
    %%
    raggedlastbottom= ##t

    #(define font-defaults
      '((font-encoding . fetaMusic)))

    %% use lmodern in latin1 (cork) flavour if EC is not available.
    #(define text-font-defaults
      `((font-encoding .
	 ,(cond
	   (tex-backend? 'Extended-TeX-Font-Encoding---Latin)
	   (else 'latin1)))
	;; add to taste here.
	
	(baseline-skip . 3)
	(word-space . 0.6)))

    #(define page-breaking optimal-page-breaks)
%    #(define page-music-height default-page-music-height )
%    #(define page-make-stencil default-page-make-stencil )

    #(define make-header (marked-up-headfoot 'oddHeaderMarkup 'evenHeaderMarkup))
    #(define make-footer (marked-up-headfoot 'oddFooterMarkup 'evenFooterMarkup))
    #(set-paper-dimension-variables (current-module))

    \include "titling-init.ly"


    topmargin = 5 \mm
    bottommargin = 6 \mm
    headsep = 4 \mm
    footsep = 4 \mm
    rightmargin = 10 \mm

    leftmargin = ##f
    firstpagenumber = #1
    printfirstpagenumber =##f
  }
