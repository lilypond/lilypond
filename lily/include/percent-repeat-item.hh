/*
  percent-repeat-item.hh -- declare Percent_repeat_item_interface

  source file of the GNU LilyPond music typesetter

  (c) 2001--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef PERCENT_REPEAT_ITEM_HH
#define PERCENT_REPEAT_ITEM_HH

#include "grob-interface.hh"
#include "lily-proto.hh"

class Percent_repeat_item_interface
{
public:
  DECLARE_GROB_INTERFACE();
  DECLARE_SCHEME_CALLBACK (beat_slash, (SCM));
  DECLARE_SCHEME_CALLBACK (double_percent, (SCM));
  static Stencil x_percent (Grob *, int);
  static Stencil brew_slash (Grob *);
};

#endif /* PERCENT_REPEAT_ITEM_HH */

