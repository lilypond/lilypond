/*
  rest-grav.cc -- implement Rest_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "item.hh"
#include "staff-symbol-referencer.hh"
#include "musical-request.hh"
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
  TRANSLATOR_DECLARATIONS(Rest_engraver);
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
      rest_ = new Item (get_property ("Rest"));

      int durlog  = unsmob_duration (rest_req_->get_mus_property ("duration"))-> duration_log ();
      
      rest_->set_grob_property ("duration-log",
				  gh_int2scm (durlog));

      int dots =unsmob_duration (rest_req_->get_mus_property ("duration"))->dot_count ();
      
      if (dots)
	{
	  dot_ = new Item (get_property ("Dots"));

	  Rhythmic_head::set_dots (rest_, dot_);
	  dot_->set_parent (rest_, Y_AXIS);
	  dot_->set_grob_property ("dot-count", gh_int2scm (dots));
	  announce_grob (dot_, SCM_EOL);
	}

      Pitch *p = unsmob_pitch (rest_req_->get_mus_property ("pitch"));

      /*
	This is ridiculous -- rests don't have pitch, but we act as if
	our nose is bleeding.
       */
      if (p)
	{
	  int pos= p->steps ();
	  SCM c0 = get_property ("centralCPosition");
	  if (gh_number_p (c0))
	    pos += gh_scm2int (c0);
	  
	  rest_->set_grob_property ("staff-position", gh_int2scm (pos));
	}
      
      announce_grob(rest_, rest_req_->self_scm());
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

ENTER_DESCRIPTION(Rest_engraver,
/* descr */       "",
/* creats*/       "Rest Dots",
/* accepts */     "rest-event",
/* acks  */      "",
/* reads */       "centralCPosition",
/* write */       "");
