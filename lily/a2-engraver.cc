/*
  a2-engraver.cc -- implement A2_engraver

  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "engraver.hh"
#include "item.hh"
#include "note-head.hh"
#include "stem.hh"
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
  enum State { NORMAL, UNISON, SOLO } state_;
};

ADD_THIS_TRANSLATOR (A2_engraver);

A2_engraver::A2_engraver ()
{
  text_p_ = 0;
  state_ = NORMAL;
}

void
A2_engraver::do_process_music ()
{
}


void
A2_engraver::acknowledge_element (Score_element_info i)
{
  if (!text_p_)
    {
      SCM unison = get_property ("unison");
      SCM solo = get_property ("solo");

      if ((solo == SCM_BOOL_T && state_ != SOLO)
	  || (unison == SCM_BOOL_T && state_ != UNISON))
	{
	  text_p_ = new Item (get_property ("basicTextScriptProperties"));
	  Side_position::set_axis (text_p_, Y_AXIS);
	  announce_element (text_p_, 0);
      
	  /*
	    Urg, read prop
	  */
	  SCM text;
	  Direction dir = UP;
	  if (solo == SCM_BOOL_T)
	    {
	      state_ = SOLO;
	      if (daddy_trans_l_->id_str_ == "one")
		text = ly_str02scm ("Solo");
	      else
		{
		  text = ly_str02scm ("Solo II");
		  dir = DOWN;
		}
	    }
	  else if (unison == SCM_BOOL_T)
	    {
	      text = ly_str02scm ("\\`a 2");
	      state_ = UNISON;
	    }
	  
	  Side_position::set_direction (text_p_, dir);
	  text_p_->set_elt_property ("text", text);

	}
    }
#if 0
}

void
A2_engraver::acknowledge_element (Score_element_info i)
{
#endif
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
	  
	  
  if (Stem::has_interface (i.elem_l_))
    {
      Item *stem_l = dynamic_cast<Item*> (i.elem_l_);

      SCM unirhythm = get_property ("unirhythm");
      SCM unison = get_property ("unison");
      SCM solo = get_property ("solo");
      SCM interval = get_property ("interval");

      /*
	This still needs some work.
       */
      if ((unirhythm != SCM_BOOL_T && solo != SCM_BOOL_T)
	  || (unirhythm == SCM_BOOL_T
	      && gh_number_p (interval) && gh_scm2int (interval) < 3))
	{
	  if (daddy_trans_l_->id_str_ == "one")
	    {
	      //Directional_element_interface (stem_l).set (UP);
	      stem_l->set_elt_property ("direction", gh_int2scm (1));
	    }
	  else if (daddy_trans_l_->id_str_ == "two")
	    {
	      //Directional_element_interface (stem_l).set (DOWN);
	      stem_l->set_elt_property ("direction", gh_int2scm (-1));
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

