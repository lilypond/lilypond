/*
  multi_measure_rest-engraver.cc -- implement Multi_measure_rest_engraver

  (c) 1998--2002 Jan Nieuwenhuizen <janneke@gnu.org>
       Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "event.hh"
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
  Music * new_req_;
  Music * busy_span_req_;
  Music * stop_req_;
  int start_measure_;
  Moment start_moment_;
  
  Spanner *mmrest_;
  Spanner *lastrest_;
};



Multi_measure_rest_engraver::Multi_measure_rest_engraver ()
{
  start_measure_ = 0;
  mmrest_  = lastrest_ =0;
  new_req_ = busy_span_req_ = stop_req_ =0;
}


bool
Multi_measure_rest_engraver::try_music (Music* req)
{
  if (req->is_mus_type ("multi-measure-rest-event"))
    {
      Direction d = to_dir (req->get_mus_property ("span-direction"));
      if (d == STOP)
	{
	  stop_req_ = req;
	}
      else if (d == START&& !new_req_)
	{
	  new_req_ = req;
	}
      return true;
    }
  return false;
}

void
Multi_measure_rest_engraver::process_music ()
{
  if (new_req_ && stop_req_)
    stop_req_ = 0;

  if (new_req_)
    start_moment_ = now_mom ();

  if (stop_req_)
    {
      busy_span_req_ =0;
      stop_req_ = 0;
    }
  
  if (new_req_)
    {
      busy_span_req_ = new_req_;
      new_req_ =0;
    }

  if (busy_span_req_ && !mmrest_)
    {
      mmrest_ = new Spanner (get_property ("MultiMeasureRest"));

      announce_grob(mmrest_, busy_span_req_->self_scm());
      start_measure_
	= gh_scm2int (get_property ("currentBarNumber"));
    }

  if (gh_string_p (get_property ("whichBar")))
    {
      Grob *cmc = unsmob_grob (get_property( "currentCommandColumn"));
      Item *it = dynamic_cast<Item*> (cmc);
      if (mmrest_)
	add_bound_item (mmrest_, it);
      if (lastrest_)
	add_bound_item (lastrest_,it);
    }
}

void
Multi_measure_rest_engraver::stop_translation_timestep ()
{
  SCM smp = get_property ("measurePosition");
  Moment mp = (unsmob_moment (smp)) ? *unsmob_moment (smp) : Moment (0);

  if (mmrest_ && (now_mom () >= start_moment_) 
      && !mp.to_bool ()
      && mmrest_->get_bound (LEFT) && mmrest_->get_bound (RIGHT))
    {
      typeset_grob (mmrest_);
      /*
	we must keep mmrest_ around to set measure-count, so
	no mmrest_ = 0 here. 
       */
    }

  if (lastrest_)
    {
      /* sanity check */
      if (lastrest_->get_bound (LEFT) && lastrest_->get_bound (RIGHT)
	  && lastrest_->get_bound (LEFT) != lastrest_->get_bound (RIGHT))
	typeset_grob (lastrest_);
      lastrest_ = 0;
    }

  if (new_req_)
    {
      busy_span_req_ = new_req_;
      new_req_ =0;
    }
  
}

void
Multi_measure_rest_engraver::start_translation_timestep ()
{
  SCM smp = get_property ("measurePosition");
  Moment mp = (unsmob_moment (smp)) ? *unsmob_moment (smp) : Moment (0);
  
  if (mmrest_ && !mp.to_bool ())
    {
      lastrest_ = mmrest_;
      int cur = gh_scm2int (get_property ("currentBarNumber"));
      lastrest_->set_grob_property ("measure-count",
				     gh_int2scm (cur - start_measure_));
      mmrest_ = 0;
    }
}


void
Multi_measure_rest_engraver::finalize ()
{
  if (mmrest_)
    typeset_grob (mmrest_);
  if (lastrest_)
    typeset_grob (lastrest_);
}

ENTER_DESCRIPTION(Multi_measure_rest_engraver,
/* descr */       "Engraves multi-measure rests that are produced with @code{R}.  Reads
measurePosition and currentBarNumber to determine what number to print over the MultiMeasureRest
",
/* creats*/       "MultiMeasureRest",
/* accepts */     "multi-measure-rest-event",
/* acks  */      "",
/* reads */       "currentBarNumber currentCommandColumn measurePosition",
/* write */       "");
