/*   
grob-pitch-tuple.hh -- declare Grob_pitch_tuple

source file of the GNU LilyPond music typesetter

(c) 2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>

 */

#ifndef GROB_PITCH_TUPLE_HH
#define GROB_PITCH_TUPLE_HH

#include "lily-proto.hh"
#include "pitch.hh"
#include "moment.hh"

struct Grob_pitch_tuple {
  Pitch pitch_;
  Grob *head_;
  Moment end_;

  Grob_pitch_tuple ();
  Grob_pitch_tuple (Grob*, Melodic_req*, Moment);
  static int pitch_compare (Grob_pitch_tuple const &, Grob_pitch_tuple const &);
  static int time_compare (Grob_pitch_tuple const &, Grob_pitch_tuple const &);  
};

int compare (Grob_pitch_tuple const &, Grob_pitch_tuple const&);


#endif /* GROB_PITCH_TUPLE_HH */

