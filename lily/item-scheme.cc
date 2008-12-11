/*
  item-scheme.cc -- implement Item bindings.

  source file of the GNU LilyPond music typesetter

  (c) 2005--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include "item.hh"


LY_DEFINE (ly_item_p, "ly:item?",
	   1, 0, 0, (SCM g),
	   "Is @var{g} an @code{Item} object?")
{
  Grob *me = unsmob_grob (g);
  bool b = dynamic_cast<Item *> (me);
  return ly_bool2scm (b);
}

LY_DEFINE (ly_item_break_dir, "ly:item-break-dir",
	   1, 0, 0, (SCM it),
	   "The break status direction of item @var{it}.  @code{-1} means"
	   " end of line, @code{0}@tie{}unbroken, and"
	   " @code{1}@tie{}beginning of line.")
{
  LY_ASSERT_TYPE (unsmob_item, it, 1);
  Item *me = unsmob_item (it);
  return scm_from_int (me->break_status_dir ());
}
