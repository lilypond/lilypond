/*
  multi_measure_rest-engraver.cc -- implement Multi_measure_rest_engraver

  (c) 1998--2002 Jan Nieuwenhuizen <janneke@gnu.org>
       Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "event.hh"
#include "multi-measure-rest.hh"
#include "paper-column.hh"
#include "engraver-group-engraver.hh"
#include "side-position-interface.hh"
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
  Spanner *number_;

  Spanner *last_number_;
  Spanner *last_rest_;
};

Multi_measure_rest_engraver::Multi_measure_rest_engraver ()
{
  start_measure_ = 0;
  mmrest_ = 0;
  last_rest_ =0;
  number_ = 0;
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
      number_ = new Spanner (get_property ("MultiMeasureRestNumber"));

      Side_position_interface::add_support (number_, mmrest_);
      number_->set_parent (mmrest_, Y_AXIS);

      announce_grob (mmrest_, busy_span_req_->self_scm());
      announce_grob (number_, busy_span_req_->self_scm());
      start_measure_
	= gh_scm2int (get_property ("currentBarNumber"));
    }

  if (gh_string_p (get_property ("whichBar")))
    {
      Grob *cmc = unsmob_grob (get_property( "currentCommandColumn"));
      Item *it = dynamic_cast<Item*> (cmc);
      if (mmrest_)
	{
	  add_bound_item (mmrest_, it);
	  add_bound_item (number_, it);
	}
      if (last_rest_)
	{
	  add_bound_item (last_rest_,it);
	  add_bound_item (last_number_, it);
	}      
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
      typeset_grob (number_);
      Side_position_interface::add_staff_support (number_);
      /*
	we must keep mmrest_ around to set measure-count, so
	no mmrest_ = 0 here. 
       */

      
    }

  if (last_rest_)
    {
      /* sanity check */
      if (last_rest_->get_bound (LEFT) && last_rest_->get_bound (RIGHT)
	  && last_rest_->get_bound (LEFT) != last_rest_->get_bound (RIGHT))
	{
	  typeset_grob (last_rest_);
	  typeset_grob (last_number_);
	}
      last_rest_ = 0;
      last_number_ = 0;
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
      last_rest_ = mmrest_;
      last_number_ = number_;

      int cur = gh_scm2int (get_property ("currentBarNumber"));
      int num = cur - start_measure_;
      last_rest_->set_grob_property ("measure-count", gh_int2scm (num));

      SCM sml = get_property ("measureLength");
      Rational ml = (unsmob_moment (sml)) ? unsmob_moment (sml)->main_part_ : Rational (1);
      if (ml >= Rational (2))
	{
	  last_rest_->set_grob_property ("use-breve-rest", SCM_BOOL_T);
	}

      mmrest_ = 0;

      SCM text =last_number_->get_grob_property ("text");
      SCM thres = get_property ("restNumberThreshold");
      int t = 1;
      if (gh_number_p (thres))
	t = gh_scm2int (thres);
      
      if (text == SCM_EOL && num <= t)
	last_number_->suicide();
      else if (text == SCM_EOL)
	{
	  text = scm_number_to_string (gh_int2scm (num), SCM_MAKINUM (10));
	  last_number_->set_grob_property ("text", text);
	}
    }
}


void
Multi_measure_rest_engraver::finalize ()
{
  if (mmrest_)
    {
      typeset_grob (mmrest_);
      typeset_grob (number_);
    }
  if (last_rest_)
    {
      typeset_grob (last_rest_);
      typeset_grob (last_number_);
    }
}

ENTER_DESCRIPTION(Multi_measure_rest_engraver,
/* descr */
		  "Engraves multi-measure rests that are produced with @code{R}.  Reads "
"measurePosition and currentBarNumber to determine what number to print "
"over the MultiMeasureRest.  Reads measureLength to determine if it "
"should use a whole rest or a breve rest to represent 1 measure "
		  ,
/* creats*/       "MultiMeasureRest MultiMeasureRestNumber",
/* accepts */     "multi-measure-rest-event",
/* acks  */      "",
/* reads */       "currentBarNumber restNumberThreshold currentCommandColumn measurePosition measureLength",
/* write */       "");
