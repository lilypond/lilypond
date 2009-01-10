/* 
  tie-specification.hh -- declare  Tie_specification
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2007--2009 Han-Wen Nienhuys <hanwen@lilypond.org>
  
*/

#ifndef TIE_SPECIFICATION_HH
#define TIE_SPECIFICATION_HH

#include "lily-proto.hh"
#include "drul-array.hh"

struct Tie_specification
{
  int position_;
  Drul_array<Grob*> note_head_drul_;
  Drul_array<int> column_ranks_;
  Grob *tie_grob_;
  
  bool has_manual_position_;
  bool has_manual_dir_;
  bool has_manual_delta_y_;
  bool has_accidental_;
  
  Real manual_position_;
  Direction manual_dir_;
  
  Tie_specification ();
  int column_span () const;
  void from_grob (Grob *);
};

#endif /* TIE_SPECIFICATION_HH */

