\version "2.23.6"

\header {

  texidoc = "The halfopenvertical articulation is available."

}
#(define mydrums '(
         (hihat         cross #f               5)
         (openhihat     cross open             5)
         (closedhihat   cross stopped          5)
         (halfopenhihat cross halfopenvertical 5)))

\new DrumStaff <<
  \set DrumStaff.drumStyleTable = #(alist->hash-table mydrums)

  \drummode { hh4 hhc hho hhho }
>>

