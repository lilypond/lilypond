/*
  a2-engraver.cc -- implement A2_engraver

  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "engraver.hh"
#include "item.hh"
#include "note-head.hh"
#include "stem.hh"
#include "slur.hh"
#include "translator-group.hh"
#include "side-position-interface.hh"
#include "directional-element-interface.hh"

class A2_engraver : public Engraver
{
public:
  A2_engraver ();
  VIRTUAL_COPY_CONS (Translator);
  
protected:
  virtual void do_process_music ();
  virtual void acknowledge_element (Score_element_info);

  virtual void do_pre_move_processing ();

private:
  Item* text_p_;
  enum State { SOLO, SPLIT_INTERVAL, UNIRHYTHM, UNISILENCE, UNISON } state_;
};

ADD_THIS_TRANSLATOR (A2_engraver);

A2_engraver::A2_engraver ()
{
  text_p_ = 0;
  state_ = UNISILENCE;
}

void
A2_engraver::do_process_music ()
{
  if (!to_boolean (get_property ("combineParts")))
    return ;
  if (!text_p_)
    {
      SCM unison = get_property ("unison");
      SCM solo = get_property ("solo");
      SCM solo_adue = get_property ("soloADue");

      if (solo_adue == SCM_BOOL_T
	  && ((solo == SCM_BOOL_T && state_ != SOLO)
	      || (unison == SCM_BOOL_T && state_ != UNISON
		  && daddy_trans_l_->id_str_ == "one")))
	{
	  text_p_ = new Item (get_property ("TextScript"));
	  Side_position::set_axis (text_p_, Y_AXIS);
	  announce_element (text_p_, 0);
      
	  Direction dir = UP;
	  SCM text;
	  if (solo == SCM_BOOL_T)
	    {
	      state_ = SOLO;
	      if (daddy_trans_l_->id_str_ == "one")
		{
		  text = get_property ("soloText");
		}
	      else
		{
		  text = get_property ("soloIIText");
		  dir = DOWN;
		}
	    }
	  else if (unison == SCM_BOOL_T)
	    {
	      state_ = UNISON;
	      if (daddy_trans_l_->id_str_ == "one")
		text = get_property ("aDueText");
	    }
	  
	  Side_position::set_direction (text_p_, dir);
	  text_p_->set_elt_property ("text", text);
	}
    }
}

void
A2_engraver::acknowledge_element (Score_element_info i)
{
  if (!to_boolean (get_property ("combineParts")))
    return ;
  
  if (text_p_)
    {
      if (Note_head::has_interface (i.elem_l_))
	{
	  Score_element*t = text_p_;
	  Side_position::add_support (t, i.elem_l_);
	  if (Side_position::get_axis (t) == X_AXIS
	      && !t->parent_l (Y_AXIS))
	    t->set_parent (i.elem_l_, Y_AXIS);
	}
      if (Stem::has_interface (i.elem_l_))
	{
	  Side_position::add_support (text_p_, i.elem_l_);
	}
    }
	  
  SCM unisilence = get_property ("unisilence");
  SCM unison = get_property ("unison");
  SCM unirhythm = get_property ("unirhythm");
  SCM solo = get_property ("solo");
  SCM split_interval = get_property ("split-interval");
  SCM solo_adue = get_property ("soloADue");
  
  State previous_state = state_;
  if (unisilence == SCM_BOOL_T)
    /*
      state_ = UNISILENCE;
    */
    ;
  else if (solo == SCM_BOOL_T)
    state_ = SOLO;
  else if (unison == SCM_BOOL_T)
    state_ = UNISON;
  else if (unirhythm == SCM_BOOL_T && split_interval == SCM_BOOL_T)
    state_ = SPLIT_INTERVAL;
  else if (unirhythm)
    state_ = UNIRHYTHM;
	  
  if (Stem::has_interface (i.elem_l_)
      || Slur::has_interface (i.elem_l_)
      // || Text_item::has_interface (i.elem_l_)
      //|| Hairpin::has_interface (i.elem_l_)
      )
    {
      /*
	Hmm.  We must set dir when solo, in order to get
	the rests collided to the right position
      */
      if ((unirhythm != SCM_BOOL_T) || (solo == SCM_BOOL_T)
	  || ((unisilence == SCM_BOOL_T && previous_state != UNISON))
	  || (unirhythm == SCM_BOOL_T && split_interval == SCM_BOOL_T
	      && (unison != SCM_BOOL_T || solo_adue != SCM_BOOL_T)))
	{
	  if (daddy_trans_l_->id_str_ == "one")
	    {
	      i.elem_l_->set_elt_property ("direction", gh_int2scm (1));
	    }
	  else if (daddy_trans_l_->id_str_ == "two")
	    {
	      i.elem_l_->set_elt_property ("direction", gh_int2scm (-1));
	    }
	}
    }
}

void 
A2_engraver::do_pre_move_processing ()
{
  if (text_p_)
    {
      Side_position::add_staff_support (text_p_);
      typeset_element (text_p_);
      text_p_ = 0;
    }
}

