/*
  hara-kiri-line-group-engraver.cc -- implement Hara_kiri_line_group_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1998 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "staff-sym.hh"
#include "command-request.hh"
#include "note-head.hh"
#include "hara-kiri-vertical-group-spanner.hh"
#include "hara-kiri-line-group-engraver.hh"

IMPLEMENT_IS_TYPE_B1 (Hara_kiri_line_group_engraver,Line_group_engraver_group);
ADD_THIS_TRANSLATOR (Hara_kiri_line_group_engraver);

void
Hara_kiri_line_group_engraver::create_line_spanner ()
{
  staffline_p_ = new Hara_kiri_vertical_group_spanner;
}

void
Hara_kiri_line_group_engraver::typeset_element(Score_element * e)
{
  if (e->is_type_b (Note_head::static_name ()))
    ((Hara_kiri_vertical_group_spanner*)staffline_p_)->add_note 
      ((Note_head*)dynamic_cast <Item *> (e));

  Line_group_engraver_group::typeset_element (e);
}

