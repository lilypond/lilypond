\version "1.3.146"

%{
Have some fun beam quanting
%}

% no y quantising
#(define (beam-vertical-position-quants m dy) '())

% rediculous dy quanting
#(define beam-height-quants '(0 4))

\score {
    \notes\relative c'{
        c8 c c c
        c8 e g a
        c,8 f b e
    }
}
