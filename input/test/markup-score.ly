\header {

    %% When vertical dimension and stacking works properly, this
    %% should be broken up into a few regression tests.

    %% Too bad that '{' is overloaded, we need something (the bit arbitrary
    %% `\score' now, to introduce to-markup-stencil-rendered music.
    
    texidoc = "Use \\score block as markup command."
    
}

\version "2.3.1"

%%;; Hmm - why is stencil so tall?
#(define (page-stack-lines page)
  (stack-stencils Y 1 0
   (map ly:paper-line-stencil (ly:page-paper-lines page))))
       
#(define (page-stack-lines page)
  (let* ((urg-stencil (ly:stencil-align-to! (stack-stencils Y DOWN 0
			(map ly:paper-line-stencil (ly:page-paper-lines page)))
		       Y DOWN))
	 (expr (ly:stencil-expr urg-stencil))
	 (xext (ly:stencil-extent urg-stencil X))
	 (yext (ly:stencil-extent urg-stencil Y)))
   (ly:stencil-align-to!
    ;; urgr
    (ly:make-stencil expr xext (cons (* -0.25 (cdr yext)) 0))
    Y CENTER)))

inBed = \paper {
    %% can't do this, this paper('s fonts) must be notified to the
    %% main/current book, and e output in the header.
    %% #(paper-set-staff-size (* 11.0 pt))
    
    raggedright = ##t
    linewidth = 0\mm
    indent = 0 \mm

    #(define page-to-stencil page-stack-lines)
    %%#(define (page-to-stencil page)
    %%  (box-stencil (page-stack-lines page) 0.1 0.5))

    \context {
	\StaffContext
	minimumVerticalExtent = ##f
    }
}

noCruft = \paper {
    \context {
	\StaffContext
	%% Hmm, no effect ?
	%% \override StaffSymbol #'print-function = ##f
	%% Clef = \turnOff
	%% StaffSymbol = \turnOff
	%% TimeSignature = \turnOff
	\override Clef #'print-function = ##f
	\override StaffSymbol #'line-count = #0
	\override TimeSignature #'print-function = ##f
    }
}

noCruftInBed = \paper {
    \inBed

    %%\noCruft
    %%URGHSr
    \context {
	\StaffContext
	%% Hmm, no effect ?
	%% \override StaffSymbol #'print-function = ##f
	%% Clef = \turnOff
	%% StaffSymbol = \turnOff
	%% TimeSignature = \turnOff
	\override Clef #'print-function = ##f
	\override StaffSymbol #'line-count = #0
	\override TimeSignature #'print-function = ##f
    }
}

tuning = \markup {
    \score { { \clef bass  <c, g, d g>1 } \paper{ \inBed }}
}

#(define-public (my-footer paper page-number)
  (let ((props (page-properties paper)))
    (interpret-markup paper props
     (markup #:fill-line ( #:line ( "Tuning: " tuning) "")))))

tempoChange = \markup {
    %% wtf, no horizontal shift?
    "" %%\kern #-10 
    \translate #'(-15 . 0)
    \score { \times 2/3 { c'8 c' c' } \paper { \noCruftInBed }}
    " ="
    \score { { c'8[ c'] } \paper { \noCruftInBed } }
}

\header {
    title = "Solo Cello Suites"
    subtitle = "Suite IV"
    subsubtitle = \markup { \fill-line < { "Originalstimmung: " \tuning } > }
}

\paper {
    #(define make-footer my-footer)
}

\relative {
    \time 4/8
    \times 2/3 { c'8 d e } \times 2/3 {c d e}
    \time 4/8
    g8^\tempoChange a8 g8 a \break
}

