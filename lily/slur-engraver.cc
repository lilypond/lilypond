/*
  slur-grav.cc -- implement Slur_engraver

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "musical-request.hh"
#include "slur-engraver.hh"
#include "slur.hh"
#include "debug.hh"
#include "note-column.hh"
#include "translator-group.hh"
#include "engraver.hh"
#include "spanner.hh"

class Slur_engraver : public Engraver
{
  Link_array<Span_req> requests_arr_;
  Link_array<Span_req> new_slur_req_l_arr_;
  Link_array<Grob> slur_l_stack_;
  Link_array<Grob> end_slur_l_arr_;
  Moment last_start_;

  void set_melisma (bool);

protected:
  virtual bool try_music (Music*);
  void deprecated_process_music ();
  virtual void acknowledge_grob (Grob_info);
  virtual void stop_translation_timestep ();
  virtual void start_translation_timestep ();
  virtual void do_removal_processing ();
  virtual void create_grobs ();

public:
  VIRTUAL_COPY_CONS (Translator);
  Slur_engraver ();
};

Slur_engraver::Slur_engraver ()
{
  last_start_ = Moment (-1);
}

bool
Slur_engraver::try_music (Music *req_l)
{
  if (Span_req *sl = dynamic_cast <Span_req *> (req_l))
    {
      String t =  ly_scm2string (sl->get_mus_property ("span-type"));
      if (t == "abort")
	{
	  for (int i = 0; i < slur_l_stack_.size (); i++)
	    {
	      slur_l_stack_[i]->suicide ();
	    }
	  slur_l_stack_.clear ();
	  for (int i = 0; i < end_slur_l_arr_.size (); i++)
	    {
	      end_slur_l_arr_[i]->suicide ();
	    }
	  end_slur_l_arr_.clear ();
	  requests_arr_.clear ();
	  new_slur_req_l_arr_.clear ();
	}
      else if (t == "slur")
	{
	  /*
	    Let's not start more than one slur per moment.
	   */
	  if (sl->get_span_dir() == START)
	    {
	      if (now_mom () > last_start_)
	        {
	          new_slur_req_l_arr_.push (sl);
		  last_start_ = now_mom ();
	          return true;
		}
	    }
	  else
	    {
	      new_slur_req_l_arr_.push (sl);
	      return true;
	    }
	}
    }
  return false;
}

void
Slur_engraver::create_grobs ()
{
  deprecated_process_music ();
}

void
Slur_engraver::set_melisma (bool m)
{
  daddy_trans_l_->set_property ("slurMelismaBusy", m ? SCM_BOOL_T :SCM_BOOL_F);
}

void
Slur_engraver::acknowledge_grob (Grob_info info)
{
  if (Note_column::has_interface (info.elem_l_))
    {
      Grob *e =info.elem_l_;
      for (int i = 0; i < slur_l_stack_.size (); i++)
	Slur::add_column (slur_l_stack_[i], e);
      for (int i = 0; i < end_slur_l_arr_.size (); i++)
	Slur::add_column (end_slur_l_arr_[i], e);
    }
}

void
Slur_engraver::do_removal_processing ()
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
  SCM wg = get_property ("weAreGraceContext");
  bool wgb = to_boolean (wg);
  if (!wgb)
    for (int i=0; i < requests_arr_.size (); i++)
      {
	requests_arr_[i]->origin ()->warning (_ ("unterminated slur"));
      }
}

void
Slur_engraver::deprecated_process_music ()
{
  Link_array<Grob> start_slur_l_arr;
  for (int i=0; i< new_slur_req_l_arr_.size (); i++)
    {
      Span_req* slur_req_l = new_slur_req_l_arr_[i];
      // end slur: move the slur to other array
      if (slur_req_l->get_span_dir() == STOP)
	{
	  if (slur_l_stack_.empty ())
	    slur_req_l->origin ()->warning (_f ("can't find start of slur"));
	  else
	    {
	      Grob* slur = slur_l_stack_.pop ();
	      SCM s = get_property ("slurEndAttachment");
	      if (gh_symbol_p (s))
		{
		  index_set_cell (slur->get_grob_property ("attachment"), STOP, s);
		}
	      end_slur_l_arr_.push (slur);
	      requests_arr_.pop ();
	    }
	}
      else  if (slur_req_l->get_span_dir() == START)
	{
	  // push a new slur onto stack.
	  // (use temp. array to wait for all slur STOPs)
	  Grob* slur = new Spanner (get_property ("Slur"));
	  Slur::set_interface (slur);
	  SCM s = get_property ("slurBeginAttachment");
	  if (gh_symbol_p (s))
	    {
	      index_set_cell (slur->get_grob_property ("attachment"), START, s);
	    }
	  start_slur_l_arr.push (slur);
	  requests_arr_.push (slur_req_l);
	  announce_grob (slur, slur_req_l);
	}
    }
  for (int i=0; i < start_slur_l_arr.size (); i++)
    slur_l_stack_.push (start_slur_l_arr[i]);
  new_slur_req_l_arr_.clear ();
}

void
Slur_engraver::stop_translation_timestep ()
{
  for (int i = 0; i < end_slur_l_arr_.size (); i++)
    {
      typeset_grob (end_slur_l_arr_[i]);
    }
  end_slur_l_arr_.clear ();
}

void
Slur_engraver::start_translation_timestep ()
{
  new_slur_req_l_arr_.clear ();
  SCM m = get_property ("automaticMelismata");
  if (to_boolean (m))
    {
      set_melisma (slur_l_stack_.size ());
    }
}


ADD_THIS_TRANSLATOR (Slur_engraver);
