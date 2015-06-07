\version "2.19.22"

\header{

  texidoc= "Tests @code{define-event-function} by creating a trivial
function converting a markup into a dynamic script post-event.  As
opposed to music functions, a direction indicator is not required."

}

\layout { ragged-right = ##t }

dynScript =
#(define-event-function (text) (markup?)
   (make-dynamic-script text))

\relative {
  c'1\dynScript pp
  c^\dynScript "fp"
  c_\dynScript "spz"
}

