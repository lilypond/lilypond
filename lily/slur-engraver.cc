/*
  slur-engraver.cc -- implement Slur_engraver

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "musical-request.hh"
#include "slur.hh"
#include "warn.hh"
#include "note-column.hh"
#include "translator-group.hh"
#include "engraver.hh"
#include "spanner.hh"

class Slur_engraver : public Engraver
{
  Link_array<Span_req> requestses_;
  Link_array<Span_req> new_slur_reqs_;
  Link_array<Grob> slur_l_stack_;
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
  if (Span_req *sl = dynamic_cast <Span_req *> (req))
    {
      String t =  ly_scm2string (sl->get_mus_property ("span-type"));
      if (t == "abort")
	{
	  for (int i = 0; i < slur_l_stack_.size (); i++)
	    {
	      slur_l_stack_[i]->suicide ();
	    }
	  slur_l_stack_.clear ();
	  for (int i = 0; i < end_slurs_.size (); i++)
	    {
	      end_slurs_[i]->suicide ();
	    }
	  end_slurs_.clear ();
	  requestses_.clear ();
	  new_slur_reqs_.clear ();
	}
      else if (t == "slur")
	{
	  /*
	    Let's not start more than one slur per moment.
	   */
	  if (sl->get_span_dir () == START)
	    {
	      if (now_mom () > last_start_)
	        {
	          new_slur_reqs_.push (sl);
		  last_start_ = now_mom ();
	          return true;
		}
	    }
	  else
	    {
	      new_slur_reqs_.push (sl);
	      return true;
	    }
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
      for (int i = 0; i < slur_l_stack_.size (); i++)
	Slur::add_column (slur_l_stack_[i], e);
      for (int i = 0; i < end_slurs_.size (); i++)
	Slur::add_column (end_slurs_[i], e);
    }
}

void
Slur_engraver::finalize ()
{
  for (int i = 0; i < slur_l_stack_.size (); i++)
    {
#if 0
      typeset_grob (slur_l_stack_[i]);
#else
      /*
	Let's not typeset unterminated stuff
       */
      slur_l_stack_[i]->suicide ();
#endif     
    }
  slur_l_stack_.clear ();

  for (int i=0; i < requestses_.size (); i++)
      {
	requestses_[i]->origin ()->warning (_ ("unterminated slur"));
      }
}

void
Slur_engraver::process_acknowledged_grobs ()
{
  Link_array<Grob> start_slurs;
  for (int i=0; i< new_slur_reqs_.size (); i++)
    {
      Span_req* slur_req = new_slur_reqs_[i];
      // end slur: move the slur to other array
      if (slur_req->get_span_dir () == STOP)
	{
	  if (slur_l_stack_.empty ())
	    /* How to shut up this warning, when Voice_devnull_engraver has
	       eaten start request? */
	    slur_req->origin ()->warning (_f ("can't find start of slur"));
	  else
	    {
	      Grob* slur = slur_l_stack_.pop ();
	    
	      end_slurs_.push (slur);
	      requestses_.pop ();
	    }
	}
      else  if (slur_req->get_span_dir () == START)
	{
	  // push a new slur onto stack.
	  // (use temp. array to wait for all slur STOPs)
	  Grob* slur = new Spanner (get_property ("Slur"));
	  Slur::set_interface (slur); // cannot remove yet!
	  start_slurs.push (slur);
	  requestses_.push (slur_req);
	  announce_grob (slur, slur_req->self_scm ());
	}
    }
  for (int i=0; i < start_slurs.size (); i++)
    slur_l_stack_.push (start_slurs[i]);
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
      set_melisma (slur_l_stack_.size ());
    }
}



ENTER_DESCRIPTION (Slur_engraver,
/* descr */       "Build slurs from Slur_reqs",
/* creats*/       "Slur",
/* acks  */       "note-column-interface",
/* reads */       "slurMelismaBusy",
/* write */       "");
