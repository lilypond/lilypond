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
String default_outname_suffix_global = "";
String default_outname_base_global =  "lelie";
int default_count_global;


void
do_scores()
{
  for (int i=0; i < global_score_array.size(); i++)
    {
//      Score *&is_p = global_score_array[i];
      Score* is_p = global_score_array[i];
      if (!is_p->header_p_)
	is_p->header_p_ = new Header;
      
                
      is_p->header_p_->lily_id_str_ = "Lily was here, " +
	get_version_number_str();
      if (is_p->errorlevel_i_)
	{
	  is_p->warning (_("Score contains errors. Will not process it. "));
	  exit_status_i_ |= 1;
	}
      else
	{
	  is_p->process();
	}
    }
}

void
clear_scores ()
{
  for (int i=0; i < global_score_array.size(); i++)
    {
      delete global_score_array[i];
    }
  global_score_array.clear();
}

void
add_score (Score * s)
{
  global_score_array.push (s);
}

