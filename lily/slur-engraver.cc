/*
  slur-engraver.cc -- implement Slur_engraver

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "request.hh"
#include "slur.hh"
#include "warn.hh"
#include "note-column.hh"
#include "translator-group.hh"
#include "engraver.hh"
#include "spanner.hh"

/*
  TODO: junk nested slur functionality.
 */
class Slur_engraver : public Engraver
{
  Link_array<Music> requests_;
  Link_array<Music> new_slur_reqs_;
  Link_array<Grob> slur_stack_;
  Link_array<Grob> end_slurs_;
  Moment last_start_;

  void set_melisma (bool);

protected:
  virtual bool try_music (Music*);
  virtual void acknowledge_grob (Grob_info);
  virtual void stop_translation_timestep ();
  virtual void start_translation_timestep ();
  virtual void finalize ();
  virtual void process_acknowledged_grobs ();

public:
  TRANSLATOR_DECLARATIONS (Slur_engraver);
};

Slur_engraver::Slur_engraver ()
{
  last_start_ = Moment (-1);
}

bool
Slur_engraver::try_music (Music *req)
{
  if (req->is_mus_type ("abort-event"))
    {
      for (int i = 0; i < slur_stack_.size (); i++)
	{
	  slur_stack_[i]->suicide ();
	}
      slur_stack_.clear ();
      for (int i = 0; i < end_slurs_.size (); i++)
	{
	  end_slurs_[i]->suicide ();
	}
      end_slurs_.clear ();
      requests_.clear ();
      new_slur_reqs_.clear ();
    }
  else if (req->is_mus_type ("slur-event"))
    {
      /*
	Let's not start more than one slur per moment.
      */
      Direction d = to_dir (req->get_mus_property ("span-direction"));
      if (d == START)
	{
	  if (now_mom () > last_start_)
	    {
	      new_slur_reqs_.push (req);
	      last_start_ = now_mom ();
	    }

	  /*
	    But we swallow other slur requests.
	  */
	      
	  return true;

	}
      else if (d == STOP)
	{
	  /*
	    Swallow other requests.
	  */
	  for (int j = new_slur_reqs_.size(); j--;)
	    {
	      Direction nd = to_dir (new_slur_reqs_[j]->get_mus_property ("span-direction"));
	      
	      if (nd == STOP)
		return true;
	    }
	      
	  new_slur_reqs_.push (req);
	  return true;
	}
    }
  return false;
}

void
Slur_engraver::set_melisma (bool m)
{
  daddy_trans_->set_property ("slurMelismaBusy", m ? SCM_BOOL_T :SCM_BOOL_F);
}

void
Slur_engraver::acknowledge_grob (Grob_info info)
{
  if (Note_column::has_interface (info.grob_))
    {
      Grob *e =info.grob_;
      for (int i = 0; i < slur_stack_.size (); i++)
	Slur::add_column (slur_stack_[i], e);
      for (int i = 0; i < end_slurs_.size (); i++)
	Slur::add_column (end_slurs_[i], e);
    }
}

void
Slur_engraver::finalize ()
{
  for (int i = 0; i < slur_stack_.size (); i++)
    {
#if 0
      typeset_grob (slur_stack_[i]);
#else
      /*
	Let's not typeset unterminated stuff
       */
      slur_stack_[i]->suicide ();
#endif     
    }
  slur_stack_.clear ();

  for (int i=0; i < requests_.size (); i++)
      {
	requests_[i]->origin ()->warning (_ ("unterminated slur"));
      }
}

void
Slur_engraver::process_acknowledged_grobs ()
{
  Link_array<Grob> start_slurs;
  for (int i=0; i< new_slur_reqs_.size (); i++)
    {
      Music* slur_req = new_slur_reqs_[i];
      // end slur: move the slur to other array
      Direction d = to_dir (slur_req->get_mus_property ("span-direction"));
   if (d== STOP)
	{
	  if (slur_stack_.empty ())
	    /* How to shut up this warning, when Voice_devnull_engraver has
	       eaten start request? */
	    slur_req->origin ()->warning (_f ("can't find start of slur"));
	  else
	    {
	      Grob* slur = slur_stack_.pop ();
	    
	      end_slurs_.push (slur);
	      requests_.pop ();
	    }
	}
      else  if (d == START)
	{
	  // push a new slur onto stack.
	  // (use temp. array to wait for all slur STOPs)
	  Grob* slur = new Spanner (get_property ("Slur"));
	  Slur::set_interface (slur); // cannot remove yet!
	  start_slurs.push (slur);
	  requests_.push (slur_req);
	  announce_grob (slur, slur_req->self_scm ());
	}
    }
  for (int i=0; i < start_slurs.size (); i++)
    slur_stack_.push (start_slurs[i]);
  new_slur_reqs_.clear ();
}

void
Slur_engraver::stop_translation_timestep ()
{
  for (int i = 0; i < end_slurs_.size (); i++)
    {
      typeset_grob (end_slurs_[i]);
    }
  end_slurs_.clear ();
}

void
Slur_engraver::start_translation_timestep ()
{
  new_slur_reqs_.clear ();
  SCM m = get_property ("automaticMelismata");
  if (to_boolean (m))
    {
      set_melisma (slur_stack_.size ());
    }
}



ENTER_DESCRIPTION (Slur_engraver,
/* descr */       "Build slurs from Slur_reqs",
/* creats*/       "Slur",
/* accepts */     "slur-event",
/* acks  */      "note-column-interface",
/* reads */       "slurMelismaBusy",
/* write */       "");
