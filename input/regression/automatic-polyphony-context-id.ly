\version "2.19.22"

\header {
  texidoc = "The bottom-level contexts in polyphony shorthand are
allocated a context id in order of creation, starting with
@code{\"1\"}.
This snippet will fail to compile if either voice has an invalid
@code{context-id} string.
"
}

assertContextId =
#(define-music-function (id) (string?)
   (let ((music (make-music 'ApplyContext
                            'procedure
                            (lambda (ctx)
                              (and
                               (not (string=? (ly:context-id ctx) id))
                               (ly:error "context-id mismatch found: expecting ~s, got ~s"
                                         id
                                         (ly:context-id ctx)))))))
     music))

\relative c'' {
  <<
    {
      \assertContextId "1"
      c4 d e2
    }
    \\
    {
      \assertContextId "2"
      a,4 b c2
    }
  >>
}
