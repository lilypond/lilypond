\version "2.15.0"
\header {
  texidoc = "
  A list of special characters ASCII aliases can be easily included.
  This works for markups and lyrics.
  "
}

\paper {
  #(include-special-characters)
}

#(define-markup-list-command (show-special-characters layout props) ()
   (let ((defs (ly:output-def-lookup layout 'text-font-defaults)))
     (interpret-markup-list layout props
       (map (lambda (pair)
         (markup #:override '(line-width . 18) #:fill-line (
             #:override '(replacement-alist . ()) (car pair)
             #:override '(thickness . 0.1) #:box (cdr pair))))
         (list-tail (assoc-get 'replacement-alist defs) 3)))))

\markup "List of the special characters:"
\markuplines \justified-lines \show-special-characters

\markup { \vspace #2 "Markup example:" }
\markup { \vspace #1 "§numero;2 §ndash; §OE;dipe§hellip; Qui de ton complexe e§s;t épargné§nnbsp;? (B. Bordage §copyright; 2011)" }
\markup { \vspace #1 "Lyric example:" }
\new Lyrics \lyricmode {
  Ce§s;16 -- §s;ez In -- fi -- dè -- les, un c§oe;ur in -- no -- cent ne §s;çau -- roit vous plai -- re~en -- cor§nnbsp;;
}
