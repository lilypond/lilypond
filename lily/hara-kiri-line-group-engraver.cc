/*
  hara-kiri-line-group-engraver.cc -- implement Hara_kiri_line_group_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1998, 1999 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "rhythmic-head.hh"
#include "hara-kiri-vertical-group-spanner.hh"
#include "hara-kiri-line-group-engraver.hh"


ADD_THIS_TRANSLATOR (Hara_kiri_line_group_engraver);

void
Hara_kiri_line_group_engraver::create_line_spanner ()
{
  staffline_p_ = new Hara_kiri_group_spanner;
  
}

void
Hara_kiri_line_group_engraver::typeset_element(Score_element * e)
{
  if (Rhythmic_head *h = dynamic_cast<Rhythmic_head *> (e))
    {
      dynamic_cast<Hara_kiri_group_spanner*> (staffline_p_)
	->add_interesting_item (h);
    }
  Line_group_engraver_group::typeset_element (e);
}

