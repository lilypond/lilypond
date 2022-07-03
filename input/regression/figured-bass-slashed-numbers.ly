\version "2.19.22"

\header {
texidoc = "Figured bass supports numbers with slashes through
them.
"
}

bassfigures = \figuremode {
  \set figuredBassPlusStrokedAlist = #'()
  <0/ 1/ 2/> <3/ 4/ 5/> <6/ 7/ 8/> <9/ 10/ 11/> <12/ 13/ 100/>
  <0\\ 1\\ 2\\> <3\\ 4\\ 5\\> <6\\ 7\\ 8\\> <9\\ 10\\ 11\\> <12\\ 13\\ 100\\>
  <3\\\+ 6\\/ 7\\+! >
}

<<
  \new FiguredBass \bassfigures
>>

%{
% Override the exceptions:

#(define (horizontal-slash-interval-default num forward number-interval mag)
    (interval-widen number-interval (* mag 0.25)))

#(define (adjust-slash-stencil-default num forward stencil mag)
    stencil)

unsetExceptions = #(define-void-function () ()
;  (set! horizontal-slash-interval horizontal-slash-interval-default)
;  (set! adjust-slash-stencil adjust-slash-stencil-default)
)

<<
  \unsetExceptions
  \new FiguredBass <<\unsetExceptions \bassfigures>>
>>
%}
