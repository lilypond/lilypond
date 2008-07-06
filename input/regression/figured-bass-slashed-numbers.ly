\version "2.11.51"

bassfigures = \figuremode {
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

unsetExceptions = #(define-music-function (parser location) ()
;  (set! horizontal-slash-interval horizontal-slash-interval-default)
;  (set! adjust-slash-stencil adjust-slash-stencil-default)
  (make-music 'Music 'void #t)
)

<<
  \unsetExceptions
  \new FiguredBass <<\unsetExceptions \bassfigures>>
>>
%}
