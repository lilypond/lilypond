/*   
'  separating-line-group-engraver.cc --  implement Separating_line_group_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "separating-group-spanner.hh"
#include "separation-item.hh"
#include "paper-column.hh"
#include "paper-def.hh"
#include "engraver.hh"
#include "axis-group-interface.hh"
#include "note-spacing.hh"

class Separating_line_group_engraver : public Engraver
{
protected:
  Item * break_malt_p_;
  Item * musical_malt_p_;
  Item * last_musical_malt_p_;

  Grob * last_note_spacing_;
  Grob * current_note_spacing_;
  Grob * staff_spacing_;
  
  Spanner * sep_span_p_;
  
  virtual void acknowledge_grob (Grob_info);
  virtual void initialize ();
  virtual void finalize ();
  virtual void stop_translation_timestep ();
public:
  TRANSLATOR_DECLARATIONS(Separating_line_group_engraver);
};

Separating_line_group_engraver::Separating_line_group_engraver ()
{
  last_note_spacing_ = 0;
  current_note_spacing_ = 0;
  staff_spacing_ =0;
  
  sep_span_p_ = 0;
  break_malt_p_ = 0;
  musical_malt_p_ =0;
}

void
Separating_line_group_engraver::initialize ()
{
  sep_span_p_ = new Spanner (get_property ("SeparatingGroupSpanner"));

  announce_grob (sep_span_p_, 0);
  sep_span_p_->set_bound (LEFT, unsmob_grob (get_property ("currentCommandColumn")));
}

void
Separating_line_group_engraver::finalize ()
{
  sep_span_p_->set_bound (RIGHT, unsmob_grob (get_property ("currentCommandColumn")));
  typeset_grob (sep_span_p_);
  sep_span_p_ =0;
}

void
Separating_line_group_engraver::acknowledge_grob (Grob_info i)
{
  Item * it = dynamic_cast <Item *> (i.grob_l_);
  if (!it)
    return;
  if (it->get_parent (X_AXIS)
      && it->get_parent (X_AXIS)
      ->has_extent_callback_b(Axis_group_interface::group_extent_callback_proc, X_AXIS))
    return;

  if (Note_spacing::has_interface (it)) 
    {
      current_note_spacing_ =  it;
      return ;
    }
  
  bool ib =Item::breakable_b (it);
  Item *&p_ref_ (ib ? break_malt_p_
		 : musical_malt_p_);

  if (!p_ref_)
    {
      p_ref_ = new Item (get_property ("SeparationItem"));

      if (ib)
	p_ref_->set_grob_property ("breakable", SCM_BOOL_T);
      announce_grob (p_ref_, 0);

      if (p_ref_ == break_malt_p_)
	{
	  staff_spacing_ = new Item (get_property ("StaffSpacing"));
	  staff_spacing_->set_grob_property ("left-item", break_malt_p_->self_scm ());
	  announce_grob (staff_spacing_, 0);

	  if (last_note_spacing_)
	    last_note_spacing_->set_grob_property ("right-item",
						   break_malt_p_->self_scm ());
	}
    }

  Separation_item::add_item (p_ref_,it);
}

void
Separating_line_group_engraver::stop_translation_timestep ()
{
  if (break_malt_p_)
    {
      Separating_group_spanner::add_spacing_unit (sep_span_p_, break_malt_p_);
      typeset_grob (break_malt_p_);

      break_malt_p_ =0;
    }

  if (staff_spacing_)
    {
      if (musical_malt_p_)
	staff_spacing_->set_grob_property ("right-item", musical_malt_p_->self_scm());

      typeset_grob (staff_spacing_);
      staff_spacing_ = 0;
    }
  
  if (musical_malt_p_)
    {
      Separating_group_spanner::add_spacing_unit (sep_span_p_, musical_malt_p_);
      typeset_grob (musical_malt_p_);
    }

  last_note_spacing_ = current_note_spacing_ ;
  current_note_spacing_ =0 ;
  
  musical_malt_p_ =0;
}





ENTER_DESCRIPTION(Separating_line_group_engraver,
/* descr */       "Generates objects for computing spacing parameters.",
/* creats*/       "SeparationItem SeparatingGroupSpanner",
/* acks  */       "grob-interface",
/* reads */       "",
/* write */       "");
