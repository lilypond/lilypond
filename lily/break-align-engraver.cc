/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/
#include "engraver.hh"
#include "protected-scm.hh"
#include "break-align-interface.hh"
#include "align-interface.hh"
#include "axis-group-interface.hh"
#include "context.hh"
#include "translator-group.hh"
#include "item.hh"

#include "translator.icc"

class Break_align_engraver : public Engraver
{
  Item *align_;
  SCM column_alist_;
  Item *left_edge_;

  void add_to_group (SCM, Item *);
  void create_alignment (Grob_info);
protected:
  void stop_translation_timestep ();
  virtual void derived_mark () const;
public:
  TRANSLATOR_DECLARATIONS (Break_align_engraver);
  DECLARE_ACKNOWLEDGER (break_aligned);
  DECLARE_ACKNOWLEDGER (break_alignable);
};

void
Break_align_engraver::stop_translation_timestep ()
{
  column_alist_ = SCM_EOL;

  align_ = 0;
  left_edge_ = 0;
}

Break_align_engraver::Break_align_engraver ()
{
  column_alist_ = SCM_EOL;
  left_edge_ = 0;
  align_ = 0;
}

void
Break_align_engraver::derived_mark () const
{
  scm_gc_mark (column_alist_);
}

void
Break_align_engraver::acknowledge_break_alignable (Grob_info inf)
{
  if (!align_)
    create_alignment (inf);

  Grob *g = inf.grob ();

  if (!g->get_parent (X_AXIS))
    g->set_parent (align_, X_AXIS);
}

void
Break_align_engraver::acknowledge_break_aligned (Grob_info inf)
{
  if (Item *item = dynamic_cast<Item *> (inf.grob ()))
    {
      /*
	Removed check for item->empty (X_AXIS). --hwn 20/1/04
      */
      if (item->get_parent (X_AXIS))
	return;

      if (!Item::is_non_musical (item))
	return;

      SCM align_name = item->get_property ("break-align-symbol");
      if (!scm_is_symbol (align_name))
	return;
	  
      if (!align_)
	create_alignment (inf);

      add_to_group (align_name, item);
    }
}

void
Break_align_engraver::create_alignment (Grob_info inf)
{
  align_ = make_item ("BreakAlignment", SCM_EOL);

  Context *origin = inf.origin_contexts (this)[0];

  Translator_group *tg = origin->implementation ();
  Engraver *random_source = dynamic_cast<Engraver *> (unsmob_translator (scm_car (tg->get_simple_trans_list ())));
  if (!random_source)
    random_source = this;

  /*
    Make left edge appear to come from same context as clef/bar-line etc.
  */
  left_edge_ = random_source->make_item ("LeftEdge", SCM_EOL);
  add_to_group (left_edge_->get_property ("break-align-symbol"),
		left_edge_);
}

void
Break_align_engraver::add_to_group (SCM align_name, Item *item)
{
  SCM s = scm_assoc (align_name, column_alist_);
  Item *group = 0;

  if (s != SCM_BOOL_F)
    {
      Grob *e = unsmob_grob (scm_cdr (s));
      group = dynamic_cast<Item *> (e);
    }
  else
    {
      group = make_item ("BreakAlignGroup", item->self_scm ());

      group->set_property ("break-align-symbol", align_name);
      group->set_parent (align_, Y_AXIS);

      column_alist_ = scm_assoc_set_x (column_alist_, align_name, group->self_scm ());

      Break_alignment_interface::add_element (align_, group);
    }
  Axis_group_interface::add_element (group, item);
}

ADD_ACKNOWLEDGER (Break_align_engraver, break_aligned);
ADD_ACKNOWLEDGER (Break_align_engraver, break_alignable);
ADD_TRANSLATOR (Break_align_engraver,
		/* doc */
		"Align grobs with corresponding @code{break-align-symbols}"
		" into groups, and order the groups according to"
		" @code{breakAlignOrder}.  The left edge of the alignment gets"
		" a separate group, with a symbol @code{left-edge}.",

		/* create */
		"BreakAlignment "
		"BreakAlignGroup "
		"LeftEdge ",

		/* read */
		"",

		/* write */
		""
		);
