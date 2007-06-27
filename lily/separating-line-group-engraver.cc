/*
  separating-line-group-engraver.cc -- implement Separating_line_group_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1998--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "engraver.hh"

#include "separating-group-spanner.hh"
#include "separation-item.hh"
#include "paper-column.hh"
#include "output-def.hh"
#include "axis-group-interface.hh"
#include "note-spacing.hh"
#include "accidental-placement.hh"
#include "context.hh"
#include "spanner.hh"
#include "grob-array.hh"
#include "pointer-group-interface.hh"

#include "translator.icc"

struct Spacings
{
  Item *staff_spacing_;
  vector<Item*> note_spacings_;

  Spacings ()
  {
    staff_spacing_ = 0;
  }

  bool is_empty () const
  {
    return !staff_spacing_ && !note_spacings_.size ();
  }
  void clear ()
  {
    staff_spacing_ = 0;
    note_spacings_.clear ();
  }
};

class Separating_line_group_engraver : public Engraver
{
protected:
  Item *break_item_;
  Item *musical_item_;
  Item *last_musical_item_;

  Spacings current_spacings_;
  Spacings last_spacings_;

  Spanner *sep_span_;

  DECLARE_ACKNOWLEDGER (item);
  void process_music ();
  virtual void finalize ();
  void stop_translation_timestep ();
  void start_translation_timestep ();
public:
  TRANSLATOR_DECLARATIONS (Separating_line_group_engraver);
};

Separating_line_group_engraver::Separating_line_group_engraver ()
{
  sep_span_ = 0;
  break_item_ = 0;
  musical_item_ = 0;
}

void
Separating_line_group_engraver::process_music ()
{
  if (!sep_span_)
    {
      sep_span_ = make_spanner ("SeparatingGroupSpanner", SCM_EOL);

      sep_span_->set_bound (LEFT, unsmob_grob (get_property ("currentCommandColumn")));
    }
}
void
Separating_line_group_engraver::finalize ()
{
  if (!sep_span_)
    return;

  SCM ccol = get_property ("currentCommandColumn");
  Grob *column = unsmob_grob (ccol);

  sep_span_->set_bound (RIGHT, unsmob_grob (ccol));
  sep_span_ = 0;

  if (last_spacings_.staff_spacing_
      && last_spacings_.staff_spacing_->get_column () == column)
    last_spacings_.staff_spacing_->suicide ();
}

void
Separating_line_group_engraver::acknowledge_item (Grob_info i)
{
  Item *it = i.item ();
  if (it->get_parent (X_AXIS)
      && it->get_parent (X_AXIS) == it->get_parent (Y_AXIS)
      && Axis_group_interface::has_axis (it->get_parent (X_AXIS), X_AXIS)
      && Axis_group_interface::has_axis (it->get_parent (Y_AXIS), Y_AXIS))
    return;

  if (to_boolean (it->get_property ("no-spacing-rods")))
    return;

  if (Note_spacing::has_interface (it))
    {
      current_spacings_.note_spacings_.push_back (it);
      return;
    }

  bool ib = Item::is_non_musical (it);
  Item *&p_ref_ (ib ? break_item_
		 : musical_item_);

  if (!p_ref_)
    {
      p_ref_ = make_item ("SeparationItem", SCM_EOL);

      if (ib)
	{
	  p_ref_->set_property ("non-musical", SCM_BOOL_T);
	  context ()->set_property ("breakableSeparationItem", p_ref_->self_scm ());
	}

      if (to_boolean (get_property ("createSpacing"))
	  && p_ref_ == break_item_)
	{
	  Item *it = make_item ("StaffSpacing", SCM_EOL);
	  current_spacings_.staff_spacing_ = it;
	  Pointer_group_interface::add_grob (it, ly_symbol2scm ("left-items"),
					     break_item_);

	  if (!last_spacings_.note_spacings_.size ()
	      && last_spacings_.staff_spacing_)
	    {
	      SCM ri = last_spacings_.staff_spacing_->get_object ("right-items");
	      Grob_array *ga = unsmob_grob_array (ri);
	      if (!ga)
		{
		  SCM ga_scm = Grob_array::make_array ();
		  last_spacings_.staff_spacing_->set_object ("right-items", ga_scm);
		  ga = unsmob_grob_array (ga_scm);
		}

	      ga->clear ();
	      ga->add (break_item_);
	    }
	}
    }

  if (Accidental_placement::has_interface (it))
    Separation_item::add_conditional_item (p_ref_, it);
  else
    Separation_item::add_item (p_ref_, it);
}

void
Separating_line_group_engraver::start_translation_timestep ()
{
  if (break_item_)
    {
      context ()->unset_property (ly_symbol2scm ("breakableSeparationItem"));
      break_item_ = 0;
    }
}

void
Separating_line_group_engraver::stop_translation_timestep ()
{
  if (break_item_)
    Separating_group_spanner::add_spacing_unit (sep_span_, break_item_);

  if (Item *sp = current_spacings_.staff_spacing_)
    {
      /*
	TODO: should really look at the left-items of following
	note-spacing grobs.
      */
      if (musical_item_)
	Pointer_group_interface::add_grob (sp, ly_symbol2scm ("right-items"),
					   musical_item_);
    }

  if (!current_spacings_.is_empty ())
    last_spacings_ = current_spacings_;

  current_spacings_.clear ();

  if (musical_item_)
    Separating_group_spanner::add_spacing_unit (sep_span_, musical_item_);

  musical_item_ = 0;
}

ADD_ACKNOWLEDGER (Separating_line_group_engraver, item);
ADD_TRANSLATOR (Separating_line_group_engraver,
		/* doc */ "Generates objects for computing spacing parameters.",

		/* create */
		"SeparationItem "
		"SeparatingGroupSpanner "
		"StaffSpacing",
		/* read */ "createSpacing",
		/* write */ "breakableSeparationItem");
