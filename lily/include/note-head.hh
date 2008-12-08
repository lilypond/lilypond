/*
  note-head.hh -- part of GNU LilyPond

  (c) 1996--2008 Han-Wen Nienhuys
*/

#ifndef NOTEHEAD_HH
#define NOTEHEAD_HH

#include "stencil.hh"
#include "grob-interface.hh"


class Note_head
{
public:
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (brew_ez_stencil, (SCM));
  DECLARE_SCHEME_CALLBACK (stem_x_shift, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_stem_attachment, (SCM));
  DECLARE_GROB_INTERFACE();

  static Real stem_attachment_coordinate (Grob *, Axis a);
  static int get_balltype (Grob *);

  static Offset get_stem_attachment (Font_metric *, string);
};
#endif // NOTEHEAD_HH

