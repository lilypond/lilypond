/*
  phrasing-slur-engraver.cc -- implement Phrasing_slur_engraver

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "musical-request.hh"
#include "slur.hh"
#include "warn.hh"
#include "note-column.hh"
#include "translator-group.hh"
#include "engraver.hh"
#include "spanner.hh"

class Phrasing_slur_engraver : public Engraver
{
  Link_array<Span_req> requestses_;
  Link_array<Span_req> new_phrasing_slur_reqs_;
  Link_array<Grob> phrasing_slur_l_stack_;
  Link_array<Grob> end_phrasing_slurs_;
  Moment last_start_;

protected:
  virtual bool try_music (Music*);
  virtual void acknowledge_grob (Grob_info);
  virtual void stop_translation_timestep ();
  virtual void start_translation_timestep ();
  virtual void finalize ();
  virtual void process_acknowledged_grobs ();

public:
  TRANSLATOR_DECLARATIONS(Phrasing_slur_engraver);
  
};

Phrasing_slur_engraver::Phrasing_slur_engraver ()
{
  last_start_ = Moment (-1);
}

bool
Phrasing_slur_engraver::try_music (Music *req)
{
  if (Span_req *sl = dynamic_cast <Span_req *> (req))
    {
      String t =  ly_scm2string (sl->get_mus_property ("span-type"));
      if (t == "abort")
	{
	  for (int i = 0; i < phrasing_slur_l_stack_.size (); i++)
	    {
	      phrasing_slur_l_stack_[i]->suicide ();
	    }
	  phrasing_slur_l_stack_.clear ();
	  for (int i = 0; i < end_phrasing_slurs_.size (); i++)
	    {
	      end_phrasing_slurs_[i]->suicide ();
	    }
	  end_phrasing_slurs_.clear ();
	  requestses_.clear ();
	  new_phrasing_slur_reqs_.clear ();
	}
      else if (t == "phrasing-slur")
	{
	  /*
	    Let's not start more than one phrasing slur per moment.
	   */
	  if (sl->get_span_dir () == START)
	    {
	      if (now_mom () > last_start_)
	        {
	          new_phrasing_slur_reqs_.push (sl);
		  last_start_ = now_mom ();
	          return true;
		}
	    }
	  else
	    {
	      new_phrasing_slur_reqs_.push (sl);
	      return true;
	    }
	}
    }
  return false;
}

void
Phrasing_slur_engraver::acknowledge_grob (Grob_info info)
{
  if (Note_column::has_interface (info.grob_))
    {
      Grob *e =info.grob_;
      for (int i = 0; i < phrasing_slur_l_stack_.size (); i++)
	Slur::add_column (phrasing_slur_l_stack_[i], e);
      for (int i = 0; i < end_phrasing_slurs_.size (); i++)
	Slur::add_column (end_phrasing_slurs_[i], e);
    }
}

void
Phrasing_slur_engraver::finalize ()
{
  for (int i = 0; i < phrasing_slur_l_stack_.size (); i++)
    {
#if 0
      typeset_grob (phrasing_slur_l_stack_[i]);
#else
      /*
	Let's not typeset unterminated stuff
       */
      phrasing_slur_l_stack_[i]->suicide ();
#endif     
    }
  phrasing_slur_l_stack_.clear ();

    for (int i=0; i < requestses_.size (); i++)
      {
	requestses_[i]->origin ()->warning (_ ("unterminated phrasing slur"));
      }
}

void
Phrasing_slur_engraver::process_acknowledged_grobs ()
{
  Link_array<Grob> start_phrasing_slurs;
  for (int i=0; i< new_phrasing_slur_reqs_.size (); i++)
    {
      Span_req* phrasing_slur_req = new_phrasing_slur_reqs_[i];
      // end phrasing slur: move the phrasing slur to other array
      if (phrasing_slur_req->get_span_dir () == STOP)
	{
	  if (phrasing_slur_l_stack_.empty ())
	    phrasing_slur_req->origin ()->warning (_f ("can't find start of phrasing slur"));
	  else
	    {
	      Grob* phrasing_slur = phrasing_slur_l_stack_.pop ();
	      end_phrasing_slurs_.push (phrasing_slur);
	      requestses_.pop ();
	    }
	}
      else  if (phrasing_slur_req->get_span_dir () == START)
	{
	  // push a new phrasing_slur onto stack.
	  // (use temp. array to wait for all phrasing_slur STOPs)
	  Grob* phrasing_slur = new Spanner (get_property ("PhrasingSlur"));
	  Slur::set_interface (phrasing_slur); // can't remove.
	  start_phrasing_slurs.push (phrasing_slur);
	  requestses_.push (phrasing_slur_req);
	  announce_grob(phrasing_slur, phrasing_slur_req->self_scm());
	}
    }
  for (int i=0; i < start_phrasing_slurs.size (); i++)
    phrasing_slur_l_stack_.push (start_phrasing_slurs[i]);
  new_phrasing_slur_reqs_.clear ();
}

void
Phrasing_slur_engraver::stop_translation_timestep ()
{
  for (int i = 0; i < end_phrasing_slurs_.size (); i++)
    {
      typeset_grob (end_phrasing_slurs_[i]);
    }
  end_phrasing_slurs_.clear ();
}

void
Phrasing_slur_engraver::start_translation_timestep ()
{
  new_phrasing_slur_reqs_.clear ();
}



ENTER_DESCRIPTION(Phrasing_slur_engraver,
/* descr */       "Print phrasing slurs. Similar to Slur_engraver",
/* creats*/       "PhrasingSlur",
/* acks  */       "note-column-interface",
/* reads */       "slurMelismaBusy",
/* write */       "");
