\version "2.15.17"

#(set-default-paper-size "a4")
\paper {
  #(include-special-characters)
}

#(define-markup-list-command (show-special-characters layout props) ()
   (let ((defs (ly:output-def-lookup layout 'text-font-defaults)))
        (interpret-markup-list layout props
          (map (lambda (pair)
            (markup #:override '(line-width . 18)
                    #:fill-line
                    (#:override '(replacement-alist . ()) (car pair)
                     #:override '(thickness . 0.1) #:box (cdr pair))))
            (list-tail (assoc-get 'replacement-alist defs) 3)))))

\markuplist \justified-lines \show-special-characters
