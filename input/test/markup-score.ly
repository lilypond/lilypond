\header {
    %% WIP
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
    raggedright = ##t
    linewidth = 40\mm
    indent = 0 \mm

    %% #(define page-to-stencil page-stack-lines)
    #(define (page-to-stencil page)
      (box-stencil (page-stack-lines page) 0.1 0.5))

    \context {
	\StaffContext
	minimumVerticalExtent = ##f
    }
}

\header {
    title = "title"
    subtitle = \markup { \fill-line <
	"subtitle with score: "
	\score { \relative \notes { a'^"Hi" b c } \paper { \inBed } }
	"woo!"
    > }
    subsubtitle = "subsubtitle"
}

\relative {
    a' b c d \break
    a b c d \break
    %% interesting bug:
    %%a b^\markup { \score{ \notes\relative{ <b'1 dis fis> } \paper{ \inBed }}} c d
    a b c d \break
    a b c d \break
}

