/*
  multi_measure_rest-engraver.cc -- implement Multi_measure_rest_engraver

  (c) 1998--2000 Jan Nieuwenhuizen <janneke@gnu.org>
       Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "musical-request.hh"
#include "multi-measure-rest.hh"
#include "paper-column.hh"
#include "engraver-group-engraver.hh"
#include "bar.hh"
#include "staff-symbol-referencer.hh"
#include "engraver.hh"
#include "moment.hh"
#include "spanner.hh"

/**
   The name says it all: make multi measure rests 

FIXME? The MM rest engraver must be able to see bar lines, so it won't
work at Voice level. Not a problem in practice, but aesthetically pleasing? 

*/
class Multi_measure_rest_engraver : public Engraver
{
public:
  VIRTUAL_COPY_CONS(Translator);
  Multi_measure_rest_engraver ();

protected:
  virtual void acknowledge_element (Score_element_info i);
  virtual void do_process_music ();
  virtual bool do_try_music (Music*);
  virtual void do_pre_move_processing ();
  virtual void do_post_move_processing ();
  virtual void do_removal_processing ();
private:
  Span_req * new_req_l_;
  Span_req * busy_span_req_l_;
  Span_req * stop_req_l_;
  int start_measure_i_;
  Moment start_moment_;
  
  Spanner *mmrest_p_;
  Spanner *lastrest_p_;
};

ADD_THIS_TRANSLATOR (Multi_measure_rest_engraver);

Multi_measure_rest_engraver::Multi_measure_rest_engraver ()
{
  start_measure_i_ = 0;
  mmrest_p_  = lastrest_p_ =0;
  new_req_l_ = busy_span_req_l_ = stop_req_l_ =0;
}

void
Multi_measure_rest_engraver::acknowledge_element (Score_element_info i)
{
  Item * item = dynamic_cast<Item*> (i.elem_l_); 
  if (item && Bar::has_interface (item))
    {
      if (mmrest_p_)
	Multi_measure_rest::add_column (mmrest_p_,item);
      if (lastrest_p_)
	Multi_measure_rest::add_column (lastrest_p_,item);
    }
}

bool
Multi_measure_rest_engraver::do_try_music (Music* req_l)
{
  if (Span_req * sp = dynamic_cast<Span_req*> (req_l))
    {
      if (sp->span_type_str_ == "rest")
	{
	  if (sp->span_dir_ == STOP)
	    {
	      stop_req_l_ = sp;
	    }
	  else if (sp->span_dir_ == START && !new_req_l_)
	    {
	      new_req_l_ = sp;
	    }
	  return true;
	}
    }
  return false;
}



void
Multi_measure_rest_engraver::do_process_music ()
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

      announce_element (mmrest_p_, busy_span_req_l_);
      start_measure_i_
	= gh_scm2int (get_property ("currentBarNumber"));
    }
}

void
Multi_measure_rest_engraver::do_pre_move_processing ()
{
  SCM smp = get_property ("measurePosition");
  Moment mp =  (unsmob_moment (smp)) ? *unsmob_moment (smp) : Moment (0);

  if (mmrest_p_ && (now_mom () >= start_moment_) 
    && !mp
    && (scm_ilength (mmrest_p_->get_elt_property ("columns")) >= 2))
    {
      typeset_element (mmrest_p_);
      /*
	must keep mmrest_p_ around to set measures_i_
       */
    }
  if (lastrest_p_)
    {
      typeset_element (lastrest_p_);
      lastrest_p_ = 0;
    }

  if (new_req_l_)
    {
      busy_span_req_l_ = new_req_l_;
      new_req_l_ =0;
    }
  
}

void
Multi_measure_rest_engraver::do_post_move_processing ()
{
  SCM smp = get_property ("measurePosition");
  Moment mp =  (unsmob_moment (smp)) ? *unsmob_moment (smp) : Moment (0);
  
  if (mmrest_p_ && !mp)
    {
      lastrest_p_ = mmrest_p_;
      int cur = gh_scm2int (get_property ("currentBarNumber"));
      lastrest_p_->set_elt_property ("measure-count",
				     gh_int2scm (cur - start_measure_i_));
      mmrest_p_ = 0;
    }
}


void
Multi_measure_rest_engraver::do_removal_processing ()
{
  if (mmrest_p_)
    typeset_element (mmrest_p_);
  if (lastrest_p_)
    typeset_element (lastrest_p_);
}
