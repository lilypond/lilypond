/*
  accidental-placement.hh -- declare  Accidental_placement

  source file of the GNU LilyPond music typesetter

  (c) 2002--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef ACCIDENTAL_PLACEMENT_HH
#define ACCIDENTAL_PLACEMENT_HH

#include "grob-interface.hh"
#include "lily-proto.hh"

class Accidental_placement
{
public:
  DECLARE_SCHEME_CALLBACK (alignment_callback, (SCM element));
  static void add_accidental (Grob *, Grob *);

  static vector<Grob*> get_relevant_accidentals (vector<Grob*> const &elts, Grob *left);
  static void split_accidentals (Grob *accs,
				 vector<Grob*> *break_reminder,
				 vector<Grob*> *real_acc);

  DECLARE_SCHEME_CALLBACK(calc_positioning_done, (SCM));
  DECLARE_GROB_INTERFACE();
};
#endif /* ACCIDENTAL_PLACEMENT_HH */

