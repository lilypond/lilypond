/*   
'  separating-line-group-engraver.cc --  implement Separating_line_group_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "separating-group-spanner.hh"
#include "separation-item.hh"
#include "paper-column.hh"
#include "paper-def.hh"
#include "engraver.hh"
#include "axis-group-interface.hh"
#include "note-spacing.hh"
#include "group-interface.hh"
#include "accidental-placement.hh"
#include "context.hh"



struct Spacings
{
  Item * staff_spacing_;
  Link_array<Item> note_spacings_;

  Spacings ()
  {
    staff_spacing_ = 0;
  }

  bool is_empty () const
  {
    return !staff_spacing_ && !note_spacings_.size (); 
  }
  void clear () {
    staff_spacing_ = 0;
    note_spacings_.clear ();
  }
};


class Separating_line_group_engraver : public Engraver
{
protected:
  Item * break_item_;
  Item * musical_item_;
  Item * last_musical_item_;

  Spacings current_spacings_;
  Spacings last_spacings_;
  
  Spanner * sep_span_;
  
  virtual void acknowledge_grob (Grob_info);
  virtual void process_music ();
  virtual void finalize ();
  virtual void stop_translation_timestep ();
  virtual void start_translation_timestep ();  
public:
  TRANSLATOR_DECLARATIONS (Separating_line_group_engraver);
};

Separating_line_group_engraver::Separating_line_group_engraver ()
{
  sep_span_ = 0;
  break_item_ = 0;
  musical_item_ =0;
}

void
Separating_line_group_engraver::process_music ()
{

  if (!sep_span_)
    {
      sep_span_ = make_spanner ("SeparatingGroupSpanner");

      announce_grob (sep_span_, SCM_EOL);
      sep_span_->set_bound (LEFT, unsmob_grob (get_property ("currentCommandColumn")));
    }
}
void
Separating_line_group_engraver::finalize ()
{
  if (!sep_span_)
    return ;
  
  SCM ccol = get_property ("currentCommandColumn");
  Grob *column = unsmob_grob (ccol);
  
  sep_span_->set_bound (RIGHT, unsmob_grob (ccol));
  typeset_grob (sep_span_);
  sep_span_ =0;

  for  (int i= 0 ; i < last_spacings_.note_spacings_.size (); i++)
    {
      Pointer_group_interface::add_grob (last_spacings_.note_spacings_[i],
					 ly_symbol2scm ("right-items" ),
					 column);
    }
   
  if (last_spacings_.staff_spacing_
     && last_spacings_.staff_spacing_->get_column () == column)
    {
      last_spacings_.staff_spacing_->suicide ();
    }
}

void
Separating_line_group_engraver::acknowledge_grob (Grob_info i)
{
  Item * it = dynamic_cast <Item *> (i.grob_);
  if (!it)
    return;
  if (it->get_parent (X_AXIS)
      && it->get_parent (X_AXIS)
      ->has_extent_callback (Axis_group_interface::group_extent_callback_proc, X_AXIS))
    return;

  
  if (to_boolean (it->get_property ("no-spacing-rods")))
    return ;

  if (Note_spacing::has_interface (it)) 
    {
      current_spacings_.note_spacings_.push (it);
      return ;
    }
  
  bool ib =Item::is_breakable (it);
  Item *&p_ref_ (ib ? break_item_
		 : musical_item_);

  if (!p_ref_)
    {
      p_ref_ = make_item ("SeparationItem");

      if (ib)
	{
	  p_ref_->set_property ("breakable", SCM_BOOL_T);
	  context ()->set_property ("breakableSeparationItem", p_ref_->self_scm ());
	}
      announce_grob (p_ref_, SCM_EOL);

      if (to_boolean (get_property ("createSpacing"))
	  && p_ref_ == break_item_)
	{
	  Item *it  = make_item ("StaffSpacing");
	  current_spacings_.staff_spacing_ = it;
	  it->set_property ("left-items", scm_cons (break_item_->self_scm (), SCM_EOL));
	  
	  announce_grob (it, SCM_EOL);

	  if (int i = last_spacings_.note_spacings_.size ())
	    {
	      for (; i--;)
		Pointer_group_interface::add_grob (last_spacings_.note_spacings_[i],
						   ly_symbol2scm ("right-items"),
						   break_item_);
				     
	    }
	  else if (last_spacings_.staff_spacing_)
	    {
	      last_spacings_.staff_spacing_->set_property ("right-items",
								scm_cons (break_item_->self_scm (), SCM_EOL));
	    }
	}
    }

  if (Accidental_placement::has_interface (it))
    Separation_item::add_conditional_item (p_ref_, it);
  else
    Separation_item::add_item (p_ref_,it);
}

void
Separating_line_group_engraver::start_translation_timestep ()
{
  if (break_item_)
    context ()->unset_property (ly_symbol2scm ("breakableSeparationItem"));
  break_item_ =0;
}

void
Separating_line_group_engraver::stop_translation_timestep ()
{
  if (break_item_)
    {
      Separating_group_spanner::add_spacing_unit (sep_span_, break_item_);
      typeset_grob (break_item_);
    }
  
  if (Item * sp = current_spacings_.staff_spacing_)
    {
      /*
	TODO: should really look at the left-items of following
	note-spacing grobs.
       */
      if (musical_item_)
	Pointer_group_interface::add_grob (sp, ly_symbol2scm ("right-items"),
					   musical_item_);

      typeset_grob (sp);
    }

  
  if (!current_spacings_.is_empty ())
    {
      last_spacings_ = current_spacings_;
    }

  current_spacings_.clear ();
  
  if (musical_item_)
    {
      Separating_group_spanner::add_spacing_unit (sep_span_, musical_item_);
      typeset_grob (musical_item_);
    }
  
  musical_item_ =0;
}


ENTER_DESCRIPTION (Separating_line_group_engraver,
/* descr */       "Generates objects for computing spacing parameters.",
/* creats*/       "SeparationItem SeparatingGroupSpanner StaffSpacing",
/* accepts */     "",
/* acks  */      "item-interface",
/* reads */       "createSpacing",
/* write */       "breakableSeparationItem");
