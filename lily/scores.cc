/*
  scores.cc -- implement some globals

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "main.hh"
#include "score.hh"
#include "string.hh"
#include "paper-def.hh"
#include "debug.hh"

static Array<Score*> score_array_global;
String default_out_fn = "lelie";

void
do_scores()
{
  for (int i=0; i < score_array_global.size(); i++) 
    {
	Score *&is_p = score_array_global[i];
	
	
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
  score_array_global.clear();
}

void
add_score (Score * s)
{
  score_array_global.push (s);
}

void
set_default_output (String s)
{
  default_out_fn = s;
}

