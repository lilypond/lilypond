/*   
  grace-position-engraver.cc --  implement Grace_position_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "engraver.hh"
#include "grace-align-item.hh"
#include "note-head.hh"
#include "local-key-item.hh"
#include "paper-column.hh"
#include "dimension-cache.hh"
#include "side-position-interface.hh"

class Grace_position_engraver:public Engraver
{
  Paper_column *last_musical_col_l_;
protected:
  VIRTUAL_COPY_CONS(Translator);
  virtual void acknowledge_element (Score_element_info);
  virtual void process_acknowledged ();
  virtual void do_post_move_processing ();
  virtual void do_pre_move_processing ();
  Grace_align_item*align_l_;
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
Grace_position_engraver::acknowledge_element (Score_element_info i)
{
  if (Grace_align_item*g  =dynamic_cast<Grace_align_item*>(i.elem_l_))
    {
      align_l_ = g;
    }
  else if (Note_head * n = dynamic_cast <Note_head*> (i.elem_l_))
    {
      if (!to_boolean (n->get_elt_property ("grace")))
	support_.push (n);
    }
  else if (Local_key_item*it = dynamic_cast<Local_key_item*>(i.elem_l_))
    {
      if (!to_boolean (it->get_elt_property ("grace")))
	support_.push (it);
      else if (align_l_) 
	it->add_dependency (align_l_);
    }
}

void
Grace_position_engraver::process_acknowledged ()
{
  if (align_l_)
    {
      for (int i=0; i < support_.size (); i++)
	side_position  (align_l_).add_support (support_[i]);
      support_.clear ();
    }
}

void
Grace_position_engraver::do_pre_move_processing ()
{
  if (align_l_ && !side_position (align_l_).supported_b ())
    {
  /*
     We don't have support. Either some moron tried attaching us to a rest,
     or we're at the end of the piece.  In the latter case, we have a
     problem if there are spanners in the grace section,
     they will want to  be broken into pieces (their line_l () field  is nil).

     Solution: attach ourselves to  the last musical column known.  A little intricate.
     
  */

      Score_element * elt = align_l_->parent_l (X_AXIS);
      if (elt)
	return;

      warning (_("Unattached grace notes.  Attaching to last musical column."));
      /*      if (ae)
	ae->remove_element (align_l_);
	else if (elt)*/

      
      align_l_->set_parent (0, X_AXIS);
      last_musical_col_l_->add_element (align_l_);
    }

  last_musical_col_l_ = get_staff_info ().musical_pcol_l ();
}

void
Grace_position_engraver::do_post_move_processing ()
{
  support_.clear ();
  align_l_ =0;
}

ADD_THIS_TRANSLATOR(Grace_position_engraver);

