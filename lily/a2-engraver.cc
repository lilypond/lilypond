/*
  a2-engraver.cc -- implement A2_engraver

  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2001 Jan Nieuwenhuizen <janneke@gnu.org>
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
  virtual void acknowledge_grob (Grob_info);
  virtual void create_grobs ();
  virtual void stop_translation_timestep ();
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
A2_engraver::create_grobs ()
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
		  && daddy_trans_l_->id_str_.left_str (3) == "one")))
	{
	  text_p_ = new Item (get_property ("TextScript"));
	  Side_position_interface::set_axis (text_p_, Y_AXIS);
	  announce_grob (text_p_, 0);
      
	  Direction dir = UP;
	  SCM text;
	  if (solo == SCM_BOOL_T)
	    {
	      state_ = SOLO;
	      if (daddy_trans_l_->id_str_.left_str (3) == "one")
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
	      if (daddy_trans_l_->id_str_.left_str (3) == "one")
		text = get_property ("aDueText");
	    }
	  
	  Side_position_interface::set_direction (text_p_, dir);
	  text_p_->set_grob_property ("text", text);
	}
    }
}

void
A2_engraver::acknowledge_grob (Grob_info i)
{
  if (!to_boolean (get_property ("combineParts")))
    return ;
  
  if (text_p_)
    {
      if (Note_head::has_interface (i.elem_l_))
	{
	  Grob*t = text_p_;
	  Side_position_interface::add_support (t, i.elem_l_);
	  if (Side_position_interface::get_axis (t) == X_AXIS
	      && !t->parent_l (Y_AXIS))
	    t->set_parent (i.elem_l_, Y_AXIS);
	}
      if (Stem::has_interface (i.elem_l_))
	{
	  Side_position_interface::add_support (text_p_, i.elem_l_);
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

  /* Must only set direction for VoiceCombines, not for StaffCombines:
     we can't detect that here, so, ugh, yet another property */
  if (!to_boolean (get_property ("noDirection"))
      && (Stem::has_interface (i.elem_l_)
	  || Slur::has_interface (i.elem_l_)
	  // || Tie::has_interface (i.elem_l_)
	  || i.elem_l_->has_interface (ly_symbol2scm ("tie-interface"))
	  
	  /*
	    Usually, dynamics are removed by *_devnull_engravers for the
	    second voice.  On the one hand, we don't want all dynamics for
	    the first voice to be placed above the staff.  On the other
	    hand, colliding of scripts may be worse.
	    So, we don't set directions for these when we're playing solo.
	  */
	  || (i.elem_l_->has_interface (ly_symbol2scm ("dynamic-interface"))
	      && state_ != SOLO)
	  || (i.elem_l_->has_interface (ly_symbol2scm ("text-interface"))
	      && state_ != SOLO)
	  ))
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
	  if (daddy_trans_l_->id_str_.left_str (3) == "one")
	    {
	      i.elem_l_->set_grob_property ("direction", gh_int2scm (1));
	    }
	  else if (daddy_trans_l_->id_str_.left_str (3) == "two")
	    {
	      i.elem_l_->set_grob_property ("direction", gh_int2scm (-1));
	    }
	}
    }
}

void 
A2_engraver::stop_translation_timestep ()
{
  if (text_p_)
    {
      Side_position_interface::add_staff_support (text_p_);
      typeset_grob (text_p_);
      text_p_ = 0;
    }
}

