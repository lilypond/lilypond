/*   
  grace-position-engraver.cc --  implement Grace_position_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "engraver.hh"
#include "grace-align-item.hh"
#include "note-head.hh"
#include "local-key-item.hh"

class Grace_position_engraver:public Engraver
{
protected:
  VIRTUAL_COPY_CONS(Translator);
  virtual void acknowledge_element (Score_element_info);
  virtual void process_acknowledged ();
  virtual void do_post_move_processing ();
  Grace_align_item*align_l_;
  Link_array<Item> support_;
public:
  Grace_position_engraver();
};


Grace_position_engraver::Grace_position_engraver ()
{
  align_l_ =0;
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
      support_.push (n);
    }
  else if (Local_key_item*it = dynamic_cast<Local_key_item*>(i.elem_l_))
    {
      if (it->get_elt_property (grace_scm_sym) == SCM_BOOL_F)
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
	align_l_->add_support (support_[i]);
      support_.clear ();
    }
}

void
Grace_position_engraver::do_post_move_processing ()
{
  support_.clear ();
  align_l_ =0;
}

ADD_THIS_TRANSLATOR(Grace_position_engraver);
