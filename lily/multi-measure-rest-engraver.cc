/*
  multi_measure_rest-engraver.cc -- implement Multi_measure_rest_engraver

  (c) 1998--2001 Jan Nieuwenhuizen <janneke@gnu.org>
       Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "musical-request.hh"
#include "multi-measure-rest.hh"
#include "paper-column.hh"
#include "engraver-group-engraver.hh"

#include "staff-symbol-referencer.hh"
#include "engraver.hh"
#include "moment.hh"
#include "spanner.hh"

/**
   The name says it all: make multi measure rests 

*/
class Multi_measure_rest_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS(Multi_measure_rest_engraver);

protected:
  virtual bool try_music (Music*);
  virtual void process_music ();
  virtual void stop_translation_timestep ();
  virtual void start_translation_timestep ();
  virtual void finalize ();

private:
  Span_req * new_req_l_;
  Span_req * busy_span_req_l_;
  Span_req * stop_req_l_;
  int start_measure_i_;
  Moment start_moment_;
  
  Spanner *mmrest_p_;
  Spanner *lastrest_p_;
};



Multi_measure_rest_engraver::Multi_measure_rest_engraver ()
{
  start_measure_i_ = 0;
  mmrest_p_  = lastrest_p_ =0;
  new_req_l_ = busy_span_req_l_ = stop_req_l_ =0;
}


bool
Multi_measure_rest_engraver::try_music (Music* req_l)
{
  if (Span_req * sp = dynamic_cast<Span_req*> (req_l))
    {
      
      if (scm_equal_p (sp->get_mus_property ("span-type"),
		       ly_str02scm ("rest")) == SCM_BOOL_T)
	{
	  if (sp->get_span_dir () == STOP)
	    {
	      stop_req_l_ = sp;
	    }
	  else if (sp->get_span_dir () == START && !new_req_l_)
	    {
	      new_req_l_ = sp;
	    }
	  return true;
	}
    }
  return false;
}

void
Multi_measure_rest_engraver::process_music ()
{
  if (new_req_l_ && stop_req_l_)
    stop_req_l_ = 0;

  if (new_req_l_)
    start_moment_ = now_mom ();

  if (stop_req_l_)
    {
      busy_span_req_l_ =0;
      stop_req_l_ = 0;
    }
  
  if (new_req_l_)
    {
      busy_span_req_l_ = new_req_l_;
      new_req_l_ =0;
    }

  if (busy_span_req_l_ && !mmrest_p_)
    {
      mmrest_p_ = new Spanner (get_property ("MultiMeasureRest"));

      Multi_measure_rest::set_interface (mmrest_p_);
      Staff_symbol_referencer::set_interface (mmrest_p_);

      announce_grob (mmrest_p_, busy_span_req_l_);
      start_measure_i_
	= gh_scm2int (get_property ("currentBarNumber"));
    }

  if (gh_string_p (get_property ("whichBar")))
    {
      Grob *cmc = unsmob_grob (get_property( "currentCommandColumn"));
      Item *it = dynamic_cast<Item*> (cmc);
      if (mmrest_p_)
	add_bound_item (mmrest_p_, it);
      if (lastrest_p_)
	add_bound_item (lastrest_p_,it);
    }
}

void
Multi_measure_rest_engraver::stop_translation_timestep ()
{
  SCM smp = get_property ("measurePosition");
  Moment mp = (unsmob_moment (smp)) ? *unsmob_moment (smp) : Moment (0);

  if (mmrest_p_ && (now_mom () >= start_moment_) 
      && !mp.to_bool ()
      && mmrest_p_->get_bound (LEFT) && mmrest_p_->get_bound (RIGHT))
    {
      typeset_grob (mmrest_p_);
      /*
	we must keep mmrest_p_ around to set measure-count, so
	no mmrest_p_ = 0 here. 
       */
    }

  if (lastrest_p_)
    {
      /* sanity check */
      if (lastrest_p_->get_bound (LEFT) && lastrest_p_->get_bound (RIGHT)
	  && lastrest_p_->get_bound (LEFT) != lastrest_p_->get_bound (RIGHT))
	typeset_grob (lastrest_p_);
      lastrest_p_ = 0;
    }

  if (new_req_l_)
    {
      busy_span_req_l_ = new_req_l_;
      new_req_l_ =0;
    }
  
}

void
Multi_measure_rest_engraver::start_translation_timestep ()
{
  SCM smp = get_property ("measurePosition");
  Moment mp = (unsmob_moment (smp)) ? *unsmob_moment (smp) : Moment (0);
  
  if (mmrest_p_ && !mp.to_bool ())
    {
      lastrest_p_ = mmrest_p_;
      int cur = gh_scm2int (get_property ("currentBarNumber"));
      lastrest_p_->set_grob_property ("measure-count",
				     gh_int2scm (cur - start_measure_i_));
      mmrest_p_ = 0;
    }
}


void
Multi_measure_rest_engraver::finalize ()
{
  if (mmrest_p_)
    typeset_grob (mmrest_p_);
  if (lastrest_p_)
    typeset_grob (lastrest_p_);
}

ENTER_DESCRIPTION(Multi_measure_rest_engraver,
/* descr */       "Engraves multi-measure rests that are produced with @code{R}.  Reads
measurePosition and currentBarNumber to determine what number to print over the MultiMeasureRest
",
/* creats*/       "MultiMeasureRest",
/* acks  */       "",
/* reads */       "currentBarNumber currentCommandColumn measurePosition",
/* write */       "");
