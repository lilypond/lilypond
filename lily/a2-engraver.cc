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
  //virtual void process_acknowledged ();

  virtual void do_pre_move_processing ();

private:
  Item* text_p_;
};

ADD_THIS_TRANSLATOR (A2_engraver);

A2_engraver::A2_engraver ()
{
  text_p_ = 0;
}

void
A2_engraver::do_process_music ()
{
  if (!text_p_)
    {
      SCM a2 = get_property ("a2");
      SCM solo = get_property ("solo");
      SCM solo2 = get_property ("solo2");

      if (solo == SCM_BOOL_T || a2 == SCM_BOOL_T || solo2 == SCM_BOOL_T)
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
	      text = ly_str02scm ("Solo");
	    }
	  else if (solo2 == SCM_BOOL_T)
	    {
	      text = ly_str02scm ("Solo II");
	      dir = DOWN;
	    }
	  else if (a2 == SCM_BOOL_T)
	    {
	      text = ly_str02scm ("\\`a 2");
	    }

	  Side_position::set_direction (text_p_, dir);
	  text_p_->set_elt_property ("text", text);

	}
    }
}

void
A2_engraver::acknowledge_element (Score_element_info i)
{
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
	  
	  SCM a2 = get_property ("a2");
	  SCM solo = get_property ("solo");
	  SCM solo2 = get_property ("solo2");

	  SCM first = get_property ("first");
	  SCM second = get_property ("second");

	  if (solo != SCM_BOOL_T
	      && solo2 != SCM_BOOL_T
	      && a2 != SCM_BOOL_T)
	    {
	      if (first == SCM_BOOL_T)
		{
		  Directional_element_interface (i.elem_l_).set (UP);
		}
	      else if (second == SCM_BOOL_T)
		{
		  Directional_element_interface (i.elem_l_).set (DOWN);
		}
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
  // burp: reset properties
  daddy_trans_l_->set_property ("a2", SCM_BOOL_F);
  daddy_trans_l_->set_property ("solo", SCM_BOOL_F);
  daddy_trans_l_->set_property ("solo2", SCM_BOOL_F);
}

