/*
  rest-grav.cc -- implement Rest_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "item.hh"
#include "staff-symbol-referencer.hh"
#include "event.hh"
#include "dots.hh"
#include "rhythmic-head.hh"
#include "engraver.hh"


class Rest_engraver : public Engraver
{
  Music *rest_req_;
  Item * dot_;
  Grob* rest_;
protected:
  virtual bool try_music (Music *);
  virtual void stop_translation_timestep ();
  virtual void start_translation_timestep ();
  virtual void process_music ();

public:
  TRANSLATOR_DECLARATIONS (Rest_engraver);
};


/*
  Should merge with Note_head_engraver
 */
Rest_engraver::Rest_engraver ()
{
  rest_req_ =0;
  rest_ =0;
  dot_ =0;
}

void
Rest_engraver::start_translation_timestep ()
{
  rest_req_ =0;
}

void
Rest_engraver::stop_translation_timestep ()
{
  if (rest_)
    {
      typeset_grob (rest_);
      rest_ =0;
    }
  if (dot_)
    {
      typeset_grob (dot_);
      dot_ =0;
    }    
}

void
Rest_engraver::process_music ()
{
  if (rest_req_ && !rest_) 
    {
      rest_ = make_item ("Rest", rest_req_->self_scm ());

      int durlog  = unsmob_duration (rest_req_->get_property ("duration"))-> duration_log ();
      
      rest_->set_property ("duration-log",
				  scm_int2num (durlog));

      int dots =unsmob_duration (rest_req_->get_property ("duration"))->dot_count ();
      
      if (dots)
	{
	  dot_ = make_item ("Dots", SCM_EOL);

	  Rhythmic_head::set_dots (rest_, dot_);
	  dot_->set_parent (rest_, Y_AXIS);
	  dot_->set_property ("dot-count", scm_int2num (dots));
	  
	}

      Pitch *p = unsmob_pitch (rest_req_->get_property ("pitch"));

      /*
	This is ridiculous -- rests don't have pitch, but we act as if
	our nose is bleeding.
       */
      if (p)
	{
	  int pos= p->steps ();
	  SCM c0 = get_property ("middleCPosition");
	  if (ly_c_number_p (c0))
	    pos += ly_scm2int (c0);
	  
	  rest_->set_property ("staff-position", scm_int2num (pos));
	}
      
    }
}

bool
Rest_engraver::try_music (Music *m)
{
  if (m->is_mus_type ("rest-event"))
    {
      rest_req_ = m;
      return true;
    }
  return false;
}

ENTER_DESCRIPTION (Rest_engraver,
/* descr */       "",
/* creats*/       "Rest Dots",
/* accepts */     "rest-event",
/* acks  */      "",
/* reads */       "middleCPosition",
/* write */       "");
