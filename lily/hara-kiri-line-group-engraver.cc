/*
  hara-kiri-line-group-engraver.cc -- implement Hara_kiri_line_group_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1998 Jan Nieuwenhuizen <jan@digicash.com>
*/

#include "staff-sym.hh"
#include "command-request.hh"
#include "p-col.hh"
#include "hara-kiri-vertical-group-spanner.hh"
#include "hara-kiri-line-group-engraver.hh"

IMPLEMENT_IS_TYPE_B1 (Hara_kiri_line_group_engraver,Engraver);
ADD_THIS_TRANSLATOR (Hara_kiri_line_group_engraver);

void
Hara_kiri_line_group_engraver::create_line_spanner ()
{
  staffline_p_ = new Hara_kiri_vertical_group_spanner;
}
