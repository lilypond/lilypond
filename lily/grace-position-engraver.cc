/*   
  grace-position-engraver.cc --  implement Grace_position_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "engraver.hh"
#include "grace-align-item.hh"
#include "rhythmic-head.hh"
#include "local-key-item.hh"
#include "paper-column.hh"
#include "note-head.hh"
#include "side-position-interface.hh"
#include "axis-group-interface.hh"


class Grace_position_engraver:public Engraver
{
  Paper_column *last_musical_col_l_;
protected:
  VIRTUAL_COPY_CONS(Translator);
  virtual void acknowledge_grob (Grob_info);
  virtual void create_grobs ();
  virtual void start_translation_timestep ();
  virtual void stop_translation_timestep ();
  Item*align_l_;
  Link_array<Item> support_;
public:
  Grace_position_engraver();
};


Grace_position_engraver::Grace_position_engraver ()
{
  align_l_ =0;
  last_musical_col_l_ =0;
}

void
Grace_position_engraver::acknowledge_grob (Grob_info i)
{
  Item *item = dynamic_cast<Item*> (i.elem_l_);
  if (item && Grace_align_item::has_interface (i.elem_l_))
    {
      align_l_ = item;
    }
  else if (item && Note_head::has_interface (i.elem_l_))
    {
      if (!to_boolean (item->get_grob_property ("grace")))
	support_.push (item);
    }
  else if (item && Local_key_item::has_interface (i.elem_l_))
    {
      if (!to_boolean (item->get_grob_property ("grace")))
	support_.push (item);
      else if (align_l_) 
	item->add_dependency (align_l_);
    }
}

void
Grace_position_engraver::create_grobs ()
{
  if (align_l_)
    {
      for (int i=0; i < support_.size (); i++)
	Side_position::add_support (align_l_,support_[i]);
      support_.clear ();
    }
}

void
Grace_position_engraver::stop_translation_timestep ()
{
  if (align_l_ && !Side_position::supported_b (align_l_))
    {
  /*
     We don't have support. Either some moron tried attaching us to a rest,
     or we're at the end of the piece.  In the latter case, we have a
     problem if there are spanners in the grace section,
     they will want to  be broken into pieces (their line_l () field  is nil).

     Solution: attach ourselves to  the last musical column known.  A little intricate.
     
  */

      Grob * elt = align_l_->parent_l (X_AXIS);
      if (elt)
	return;

      if (last_musical_col_l_)
	{
	  warning (_("Unattached grace notes.  Attaching to last musical column."));
      
	  align_l_->set_parent (0, X_AXIS);
	  Axis_group_interface::add_element (last_musical_col_l_, align_l_);
	}
      else
	{
	  // tja.
	}
    }
  last_musical_col_l_ = dynamic_cast<Paper_column*>( unsmob_grob (get_property ("currentMusicalColumn")));
}

void
Grace_position_engraver::start_translation_timestep ()
{
  support_.clear ();
  align_l_ =0;
}

ADD_THIS_TRANSLATOR(Grace_position_engraver);

