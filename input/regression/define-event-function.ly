\version "2.15.13"

\header{

  texidoc= "Tests @code{define-event-function} by creating a trivial
function converting a markup into a dynamic script post-event.  As
opposed to music functions, a direction indicator is not required."

}

\layout { ragged-right = ##t }

dynScript =
#(define-event-function (parser location text) (markup?)
   (make-dynamic-script text))

\relative c' {
  c1\dynScript pp
  c^\dynScript "fp"
  c_\dynScript "spz"
}

