\version "2.19.22"

\header
{

  texidoc = "Music functions may be attached to notes;
in this case they must be introduced by a direction
indicator.  If a non-neutral direction is given (i.e.
anything else than a dash), then the 'direction property
of the resulting object is set accordingly."

}

dynScript =
#(define-music-function (text) (string?)
   (make-dynamic-script text))

\relative {
  c'1-\dynScript "pp"
  c^\dynScript "fp"
  c_\dynScript "spz"
}
