\version "1.7.18"
% possible rename to scheme- or something like that.  -gp
\header { texidoc = "@cindex Scheme Move Text
You can move objects around with scheme.  This example shows how to
move text around. " }

#(define (make-text-checker text)
   (lambda (grob) (equal? text (ly:get-grob-property grob 'text))))

\score {
  \notes\relative c''' {
    \property Voice.Stem \set #'direction = #1
    \outputproperty #(make-text-checker (make-simple-markup "m.d."))
      #'extra-offset = #'(-3.5 . -4.5)
    a^2^"m.d."
  }
  \paper { raggedright = ##t}
}
%% new-chords-done %%
