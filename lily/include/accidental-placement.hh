/*
  accidental-placement.hh -- declare  Accidental_placement

  source file of the GNU LilyPond music typesetter

  (c) 2002--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef ACCIDENTAL_PLACEMENT_HH
#define ACCIDENTAL_PLACEMENT_HH

#include "grob.hh"

class Accidental_placement
{
public:
  DECLARE_SCHEME_CALLBACK (alignment_callback, (SCM element));
  static void add_accidental (Grob *, Grob *);

  static Interval get_relevant_accidental_extent (Grob *me,
						  Item *item_col,
						  Grob *acc);
  static void split_accidentals (Grob *accs,
				 vector<Grob*> *break_reminder,
				 vector<Grob*> *real_acc);

  DECLARE_SCHEME_CALLBACK(calc_positioning_done, (SCM));
  static bool has_interface (Grob *);
};
#endif /* ACCIDENTAL_PLACEMENT_HH */

