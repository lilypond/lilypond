
\version "2.1.26"

\header{
texidoc="
By inserting @TeX{} commands between systems, you can force pagebreaks.

In reality, you'd use the LateX command @code{\\newpage} instead of (pagebreak)
of course. 
"
}


#(define (set-page-break grob grob-c context)
  (let*
   ((meta (ly:grob-property grob 'meta))
    (name (cdr (assoc 'name meta))))
   
   (if (equal? 'NonMusicalPaperColumn name)
    (ly:grob-set-property! grob 'between-system-string "(pagebreak)\n\n"))
))

\score {
    \notes \relative c' {
	c1
	\context Score \applyoutput #set-page-break
	\break
	
	c1
    }
}

