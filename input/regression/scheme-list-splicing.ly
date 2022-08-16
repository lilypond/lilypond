\version "2.23.12"

\header {
  texidoc = "The @code{#@@} and @code{$@@} operators splice a list, returning
multiple values to the parser.  This is equivalent to returning the multiple
values directly using @code{values}."
}

{
  %% This should print a scale.
  #@(map (lambda (i)
           #{ $(ly:make-pitch 0 i) 4 #})
         (iota 8))
  %% This should print all notes of the scale in a chord.
  <
    $@(map (lambda (i)
             #{ $(ly:make-pitch 0 i) 4 #})
           (iota 8))
  >
}

%% Test #@ and $@ in embedded LilyPond #{ ... #}.
%% This score should look the same as the previous one.

scale =
#(define-music-function (n) (index?)
   #{ #@(map (lambda (i)
               #{ $(ly:make-pitch 0 i) 4 #})
             (iota n)) #})

chord =
#(define-music-function (n) (index?)
   #{
     <
       $@(map (lambda (i)
                #{ $(ly:make-pitch 0 i) 4 #})
              (iota n))
     >
   #})

{
  \scale 8
  \chord 8
}

%% This score should also look exactly the same.

chordII =
#(define-music-function (n) (index?)
   #{
     <
       #(apply values
               (map (lambda (i)
                      #{ $(ly:make-pitch 0 i) 4 #})
                    (iota n)))
     >
   #})

{
  $(apply values
          (map (lambda (i)
                 #{ $(ly:make-pitch 0 i) 4 #})
               (iota 8)))
  \chordII 8
}
