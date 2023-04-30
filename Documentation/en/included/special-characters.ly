\version "2.16.0"

\paper {
  #(include-special-characters)
}

#(define-markup-list-command (show-special-characters layout props) ()
   (interpret-markup-list
    layout props
    (map (lambda (pair)
           (markup #:override '(line-width . 18)
                   #:fill-line
                   (#:override '(replacement-alist . ()) (car pair)
                    #:override '(thickness . 0.1) #:box (cdr pair))))
         (list-tail (chain-assoc-get 'replacement-alist props) 3))))

\markuplist \justified-lines \show-special-characters
