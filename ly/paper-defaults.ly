\version "2.3.22"

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
    
    #(define-public score-title default-score-title)
    #(define-public user-title default-user-title)
    #(define-public book-title default-book-title)

    %%
    %% ugh. hard coded?
    %%

    #(layout-set-staff-size (* 20.0 pt))

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


    raggedbottom = ##f

    %%
    %% looks best for shorter scores.
    %%
    raggedlastbottom= ##t

    %% ugh.  Should use /etc/papersize and set explicitely for
    %% documentation.
    papersize = "a4"

    #(define font-defaults
      '((font-encoding . fetaMusic)))

    #(define text-font-defaults
      '((font-encoding . ec)
	(baseline-skip . 2)
	(word-space . 0.6)))

    #(define page-breaking ly:optimal-page-breaks)
    #(define page-music-height default-page-music-height )
    #(define page-make-stencil default-page-make-stencil )

    #(define make-header plain-header)
    #(define make-footer plain-footer)
    #(set-paper-dimension-variables (current-module))
}
