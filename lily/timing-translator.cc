/*
  timing-translator.cc -- implement Timing_translator

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "timing-translator.hh"
#include "command-request.hh"
#include "translator-group.hh"
#include "global-translator.hh"
#include "multi-measure-rest.hh"

Timing_translator::Timing_translator ()
{
  default_grouping_ = Rhythmic_grouping (MInterval (0, 1), 4); // ugh
}

bool
Timing_translator::do_try_request(Request*r)
{
  Command_req * c = r->access_Command_req ();
  if (!(c && c->access_Timing_req ()))
    return false;
  for (int i=0; i < timing_req_l_arr_.size (); i++)
    {
      if (timing_req_l_arr_[i]->equal_b(r))
	return true;
      if (timing_req_l_arr_[i]->name() == r->name())
	{
	  r->warning (_ ("conflicting timing request"));
	  return false;
	}
    }

  timing_req_l_arr_.push(c->access_Timing_req ());
  return true;
}

Time_signature_change_req*
Timing_translator::time_signature_req_l() const
{
  Time_signature_change_req *m_l=0;
  for (int i=0; !m_l && i < timing_req_l_arr_.size (); i++)
    {
      m_l=timing_req_l_arr_[i]->access_Time_signature_change_req ();
    }
  return m_l;
}

void
Timing_translator::do_process_requests()
{
  for (int i=0; i < timing_req_l_arr_.size (); i++)
    {
      Timing_req * tr_l = timing_req_l_arr_[i];
      Time_signature_change_req *m_l = tr_l->access_Time_signature_change_req ();
      if (m_l)
	{
	  int b_i= m_l->beats_i_;
	  int o_i = m_l->one_beat_i_;
	  if (! time_.allow_time_signature_change_b())
	    tr_l->warning (_ ("time signature change not allowed here"));
	  else
	    {
	      time_.set_time_signature (b_i, o_i);
	      default_grouping_ =
		Rhythmic_grouping (MInterval (0,Moment (b_i, o_i)), b_i);
	    }
	}
      else if (tr_l->access_Partial_measure_req ())
	{
	  Moment m = tr_l->access_Partial_measure_req ()->duration_;
	  String error = time_.try_set_partial_str (m);
	  if (error.length_i ())
	    {
	      tr_l->warning (error);
	    }
	  else
	    time_.setpartial (m);
	}
      else if (tr_l->access_Barcheck_req())
	{
	  if (time_.whole_in_measure_)
	    {
	      tr_l ->warning (_f ("barcheck failed by: %s", 
	        time_.whole_in_measure_.str ()));

	      time_.whole_in_measure_ = 0; // resync
	      time_.error_b_ = true;
	    }

	}
      else if (tr_l->access_Cadenza_req ())
	{
	  time_.set_cadenza (tr_l->access_Cadenza_req ()->on_b_);
	}
      else if (tr_l->access_Measure_grouping_req ())
	{
	  default_grouping_ =
	    parse_grouping (tr_l->access_Measure_grouping_req ()->beat_i_arr_,
			    tr_l->access_Measure_grouping_req ()->elt_length_arr_);

	}
    }
}


void
Timing_translator::do_pre_move_processing()
{
  timing_req_l_arr_.set_size (0);
  Global_translator *global_l =
    daddy_trans_l_->ancestor_l (100)->global_l (); // ugh 100.


  /* allbars == ! skipbars */
  bool allbars = ! get_property ("SkipBars").to_bool ();


  if (!time_.cadenza_b_ && allbars)
    global_l->add_moment_to_process (time_.next_bar_moment ());
}

IMPLEMENT_IS_TYPE_B1(Timing_translator, Translator);
ADD_THIS_TRANSLATOR(Timing_translator);

void
Timing_translator::do_creation_processing()
{
  time_.when_ = now_moment ();
}

void
Timing_translator::do_post_move_processing()
{
  time_.add (now_moment ()  - time_.when_);
}

void
Timing_translator::do_print () const
{
#ifndef NPRINT
  time_.print ();
  default_grouping_.print ();
#endif
}
