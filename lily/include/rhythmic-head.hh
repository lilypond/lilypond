/*
  rhythmic-head.hh -- declare Rhythmic_head

  source file of the GNU LilyPond music typesetter

  (c) 1997--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef RHYTHMIC_HEAD_HH
#define RHYTHMIC_HEAD_HH

#include "grob-interface.hh"
#include "lily-proto.hh"

class Rhythmic_head
{
public:
  static int duration_log (Grob *);
  static void set_dots (Grob *, Item *);
  static Item *get_stem (Grob *);
  static Item *get_dots (Grob *);
  static int dot_count (Grob *);
  DECLARE_SCHEME_CALLBACK (after_line_breaking, (SCM));
  DECLARE_GROB_INTERFACE();
};

#endif // RHYTHMIC_HEAD_HH
