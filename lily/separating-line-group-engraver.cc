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


struct Spacings
{
  Item * staff_spacing_;
  Link_array<Item> note_spacings_;

  Spacings ()
  {
    staff_spacing_ = 0;
  }

  bool empty( )const
  {
    return !staff_spacing_ && !note_spacings_.size (); 
  }
  void clear () {
    staff_spacing_ = 0;
    note_spacings_.clear();
  }
};

class Separating_line_group_engraver : public Engraver
{
protected:
  Item * break_malt_p_;
  Item * musical_malt_p_;
  Item * last_musical_malt_p_;

  Spacings current_spacings_;
  Spacings last_spacings_;
  
  Spanner * sep_span_p_;
  
  virtual void acknowledge_grob (Grob_info);
  virtual void initialize ();
  virtual void finalize ();
  virtual void stop_translation_timestep ();
  virtual void start_translation_timestep ();  
public:
  TRANSLATOR_DECLARATIONS(Separating_line_group_engraver);
};

Separating_line_group_engraver::Separating_line_group_engraver ()
{
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
  SCM ccol = get_property ("currentCommandColumn");
  sep_span_p_->set_bound (RIGHT, unsmob_grob (ccol));
  typeset_grob (sep_span_p_);
  sep_span_p_ =0;

  for  (int i= 0 ; i < last_spacings_.note_spacings_.size(); i++)
    {
      last_spacings_.note_spacings_[i]->set_grob_property ("right-items", gh_cons (ccol, SCM_EOL));
    }

  if(last_spacings_.staff_spacing_
     && last_spacings_.staff_spacing_->column_l () == unsmob_grob (ccol))
    {
      last_spacings_.staff_spacing_->suicide ();
    }
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
      current_spacings_.note_spacings_.push (it);
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
	  Item *it  = new Item (get_property ("StaffSpacing"));
	  current_spacings_.staff_spacing_ = it;
	  it->set_grob_property ("left-items", gh_cons (break_malt_p_->self_scm (), SCM_EOL));
	  
	  announce_grob (it, 0);

	  if (int i = last_spacings_.note_spacings_.size ())
	    {
	      SCM break_malt = gh_cons (break_malt_p_->self_scm (), SCM_EOL);
	      for (; i--;)
		last_spacings_.note_spacings_[i]
		  ->set_grob_property ("right-items",break_malt);
				     
	    }
	  else if (last_spacings_.staff_spacing_)
	    {
	      
	      last_spacings_.staff_spacing_->set_grob_property ("right-items",
								gh_cons (break_malt_p_->self_scm(), SCM_EOL));
	    }
	}
    }

  Separation_item::add_item (p_ref_,it);
}

void
Separating_line_group_engraver::start_translation_timestep ()
{

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

  if (Item * sp = current_spacings_.staff_spacing_)
    {
      /*
	TODO: should really look at the left-items of following
	note-spacing grobs.
       */
      if (musical_malt_p_)
	sp->set_grob_property ("right-items", musical_malt_p_->self_scm());

      typeset_grob (sp);
    }

  if (!current_spacings_.empty ())
    {
      last_spacings_ = current_spacings_;
    }

  current_spacings_.clear ();
  
  if (musical_malt_p_)
    {
      Separating_group_spanner::add_spacing_unit (sep_span_p_, musical_malt_p_);
      typeset_grob (musical_malt_p_);
    }
  
  musical_malt_p_ =0;
}





ENTER_DESCRIPTION(Separating_line_group_engraver,
/* descr */       "Generates objects for computing spacing parameters.",
/* creats*/       "SeparationItem SeparatingGroupSpanner",
/* acks  */       "grob-interface",
/* reads */       "",
/* write */       "");
