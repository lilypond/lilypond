/*
  a2-engraver.cc -- implement A2_engraver

  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2002 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "engraver.hh"
#include "item.hh"
#include "note-head.hh"
#include "stem.hh"
#include "slur.hh"
#include "translator-group.hh"
#include "side-position-interface.hh"
#include "directional-element-interface.hh"
#include "multi-measure-rest.hh"
#include "tie.hh"

class A2_engraver : public Engraver
{
  TRANSLATOR_DECLARATIONS(A2_engraver);

protected:
  virtual void acknowledge_grob (Grob_info);
  virtual void create_grobs ();
  virtual void stop_translation_timestep ();
private:
  Item* text_p_;
  enum State { SOLO, SPLIT_INTERVAL, UNIRHYTHM, UNISILENCE, UNISON } state_;
};



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
	  announce_grob(text_p_, SCM_EOL);
      
	  Direction dir = UP;
	  SCM text = SCM_EOL;
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
      if (Note_head::has_interface (i.grob_l_))
	{
	  Grob*t = text_p_;
	  Side_position_interface::add_support (t, i.grob_l_);
	  if (Side_position_interface::get_axis (t) == X_AXIS
	      && !t->get_parent (Y_AXIS))
	    t->set_parent (i.grob_l_, Y_AXIS);
	}
      if (Stem::has_interface (i.grob_l_))
	{
	  Side_position_interface::add_support (text_p_, i.grob_l_);
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

  Direction d = CENTER;
  if (daddy_trans_l_->id_str_.left_str (3) == "one")
    d =  UP;
  else if (daddy_trans_l_->id_str_.left_str (3) == "two")
    d = DOWN;

  /* Must only set direction for VoiceCombines, not for StaffCombines:
     we can't detect that here, so, ugh, yet another property */
  if (!to_boolean (get_property ("noDirection"))
      && (Stem::has_interface (i.grob_l_)
	  || Slur::has_interface (i.grob_l_)
	  || Tie::has_interface (i.grob_l_)
	  /*
	    Usually, dynamics are removed by *_devnull_engravers for the
	    second voice.  On the one hand, we don't want all dynamics for
	    the first voice to be placed above the staff.  On the other
	    hand, colliding of scripts may be worse.
	    So, we don't set directions for these when we're playing solo.
	  */
	  || (i.grob_l_->internal_has_interface (ly_symbol2scm ("dynamic-interface"))
	      && state_ != SOLO)
	  || (i.grob_l_->internal_has_interface (ly_symbol2scm ("text-interface"))
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
	
	  /*
	    Blunt axe method: every grob gets a propertysetting.
	   */
	  i.grob_l_->set_grob_property ("direction", gh_int2scm (d));
	}
    }

  /*
    todo: should we have separate state variable for being "rest while
    other has solo?"  */
  if ( Multi_measure_rest::has_interface (i.grob_l_) && d )
    if (state_ == UNIRHYTHM
	&& unisilence != SCM_BOOL_T)
    {
      i.grob_l_->set_grob_property ("staff-position", gh_int2scm (d * 6));
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

ENTER_DESCRIPTION(A2_engraver,
/* descr */       "Part combine engraver for orchestral scores.

The markings @emph{a2}, @emph{Solo} and @emph{Solo II}, are
created by this engraver.  It also acts upon instructions of the part
combiner.  Another thing that the this engraver, is forcing of stem,
slur and tie directions, always when both threads are not identical;
up for the musicexpr called @code{one}, down for the musicexpr called
@code{two}.

",
/* creats*/       "TextScript",
/* acks  */       "grob-interface tie-interface note-head-interface ",
/* reads */       "combineParts noDirection soloADue soloText soloIIText aDueText split-interval unison solo unisilence unirhythm",
/* write */       "");
