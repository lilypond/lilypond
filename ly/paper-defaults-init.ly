\version "2.11.11"
#(use-modules (scm layout-page-layout))
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
    %%%%input-encoding = #"TeX"
    input-encoding = #"latin1"
    print-page-number = ##t

    %%
    %% 20pt staff, 5 pt = 1.75 mm
    %%

    output-scale = #1.7573
    
    #(define-public book-title (marked-up-title 'bookTitleMarkup))
    #(define-public score-title (marked-up-title 'scoreTitleMarkup))
    #(define-public force-eps-font-include #f)
    
    %%
    %% ugh. hard coded?
    %%

    #(layout-set-absolute-staff-size (* 20.0 pt))


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
    between-system-space = #(* 20 mm)
    
    
    %%
    %% fixed space between systems.
    %%
    between-system-padding = #(* 4 mm)

    after-title-space = 5 \mm
    before-title-space = 10 \mm
    between-title-space = 2 \mm


    %%
    %% Small staves are aligned so they come out on the same place on
    %% across different pages.
    %%
    page-top-space = #(* 12 mm)

    
    ragged-bottom = ##f

    %%
    %% looks best for shorter scores.
    %%
    ragged-last-bottom= ##t

    %%
    %% settings for the page breaker
    %%
    blank-last-page-force = 0
    blank-after-score-page-force = 2
    blank-page-force = 5

    %%
    %% To limit space between systems on a page with a lot of space left
    %%
    page-limit-inter-system-space = ##f
    page-limit-inter-system-space-factor = 1.4

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

    #(define page-breaking ly:optimal-breaking)
    #(define page-breaking-wrapper page-breaking-wrapper)
    #(define page-post-process post-process-pages)

    #(define write-page-layout (ly:get-option 'dump-tweaks))
    #(define system-maximum-stretch-procedure
       (lambda (line)
	 (if (stretchable-line? line)
	     (let ((height (line-height line)))
	       (/ (* height height) 80.0))
	     0.0)))

%    #(define page-music-height default-page-music-height )
%    #(define page-make-stencil default-page-make-stencil )

    #(define make-header (marked-up-headfoot 'oddHeaderMarkup 'evenHeaderMarkup))
    #(define make-footer (marked-up-headfoot 'oddFooterMarkup 'evenFooterMarkup))
    #(set-paper-dimension-variables (current-module))

    \include "titling-init.ly"

    top-margin = 5 \mm
    bottom-margin = 6 \mm
    head-separation = 4 \mm
    foot-separation = 4 \mm

    first-page-number = #1
    print-first-page-number =##f
  }
