/*
  slur-engraver.cc -- implement Slur_engraver

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "event.hh"
#include "slur.hh"
#include "warn.hh"
#include "note-column.hh"
#include "context.hh"

#include "engraver.hh"
#include "spanner.hh"

/*
  TODO: junk nested slur functionality.
 */
class Slur_engraver : public Engraver
{
  Link_array<Music> events_;
  Link_array<Music> new_slur_evs_;
  Link_array<Grob> slur_stack_;
  Link_array<Grob> end_slurs_;
  Moment last_start_;

  void set_melisma (bool);

protected:
  virtual bool try_music (Music*);
  virtual void acknowledge_grob (Grob_info);
  virtual void stop_translation_timestep ();
  virtual void finalize ();
  virtual void process_music ();

public:
  TRANSLATOR_DECLARATIONS (Slur_engraver);
};

Slur_engraver::Slur_engraver ()
{
  last_start_ = Moment (-1);
}

bool
Slur_engraver::try_music (Music *ev)
{
  if (ev->is_mus_type ("slur-event"))
    {
      /*
	Let's not start more than one slur per moment.
      */
      Direction d = to_dir (ev->get_property ("span-direction"));
      if (d == START)
	{
	  if (now_mom () > last_start_)
	    {
	      new_slur_evs_.push (ev);
	      last_start_ = now_mom ();
	    }

	  /*
	    But we swallow other slur events.
	  */
	      
	  return true;
	}
      else if (d == STOP)
	{
	  /*
	    Swallow other events.
	  */
	  for (int j = new_slur_evs_.size (); j--;)
	    {
	      Direction nd = to_dir (new_slur_evs_[j]->get_property ("span-direction"));
	      
	      if (nd == STOP)
		return true;
	    }
	      
	  new_slur_evs_.push (ev);
	  return true;
	}
    }
  return false;
}

void
Slur_engraver::set_melisma (bool m)
{
  context ()->set_property ("slurMelismaBusy", m ? SCM_BOOL_T :SCM_BOOL_F);
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
      /*
	Let's not typeset unterminated stuff
       */
      slur_stack_[i]->suicide ();
    }
  slur_stack_.clear ();

  for (int i=0; i < events_.size (); i++)
      {
	events_[i]->origin ()->warning (_ ("unterminated slur"));
      }
}

void
Slur_engraver::process_music ()
{
  Link_array<Grob> start_slurs;
  for (int i=0; i< new_slur_evs_.size (); i++)
    {
      Music* slur_ev = new_slur_evs_[i];
      // end slur: move the slur to other array
      Direction d = to_dir (slur_ev->get_property ("span-direction"));
      if (d== STOP)
	{
	  if (slur_stack_.is_empty ())
	    /* How to shut up this warning, when Voice_devnull_engraver has
	       eaten start event? */
	    slur_ev->origin ()->warning (_f ("can't find start of slur"));
	  else
	    {
	      Grob* slur = slur_stack_.pop ();
	    
	      end_slurs_.push (slur);
	      events_.pop ();
	    }
	}
      else  if (d == START)
	{
	  // push a new slur onto stack.
	  // (use temp. array to wait for all slur STOPs)
	  Grob* slur = make_spanner ("Slur", slur_ev->self_scm ());
	  Slur::set_interface (slur); // cannot remove yet!


	  if (Direction updown = to_dir (slur_ev->get_property ("direction")))
	    {
	      slur->set_property ("direction", scm_int2num (updown));
	    }
	  
	  start_slurs.push (slur);
	  events_.push (slur_ev);
	}
    }

  slur_stack_.concat  (start_slurs);

  set_melisma (slur_stack_.size ());

  new_slur_evs_.clear ();
}

void
Slur_engraver::stop_translation_timestep ()
{
  for (int i = 0; i < end_slurs_.size (); i++)
    {
      typeset_grob (end_slurs_[i]);
    }
  end_slurs_.clear ();
  new_slur_evs_.clear ();
}



ENTER_DESCRIPTION (Slur_engraver,
/* descr */       "Build slurs from Slur_evs",
/* creats*/       "Slur",
/* accepts */     "slur-event",
/* acks  */      "note-column-interface",
/* reads */       "slurMelismaBusy",
/* write */       "");
