/*
  scores.cc -- implement some globals

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "main.hh"
#include "score.hh"
#include "string.hh"
#include "paper-def.hh"
#include "header.hh"
#include "debug.hh"
#include "parray.hh"

static Link_array<Score> global_score_array;
String default_out_fn = "lelie";

void
do_scores()
{
  for (int i=0; i < global_score_array.size(); i++) 
    {
	Score *&is_p = global_score_array[i];
	if (is_p->header_p_)
	  is_p->header_p_->lily_id_str_ = "Lily was here, " + 
	    get_version_number_str();
	if (is_p->errorlevel_i_) 
	  {
	    is_p->warning ("Score contains errors. Will not process it. ");
	    exit_status_i_ |= 1;
	  }
	else 
	  {
	    is_p->print();
	    is_p->process();
	  }
	delete is_p;
	is_p =0;

    }
  global_score_array.clear();
}

void
add_score (Score * s)
{
  global_score_array.push (s);
}

void
set_default_output (String s)
{
  default_out_fn = s;
}

