//
// mudela-score.cc -- implement Mudela_score
//
// copyright 1997 Jan Nieuwenhuizen <janneke@gnu.org>

#include <assert.h>
#include "moment.hh"
#include "duration.hh"
#include "duration-convert.hh"
#include "mi2mu-global.hh"
#include "mudela-column.hh"
#include "mudela-item.hh"
#include "mudela-score.hh"
#include "mudela-staff.hh"
#include "mudela-stream.hh"

// ugh, cygnus' b19 gcc
#include "list.tcc"
#include "cursor.tcc"

//static Mudela_key key_c (0, 0);
static Mudela_time_signature time_sig_4 (4, 2, 24, 8);
// useconds per 4: 250000 === 60 4 per minute
static Mudela_tempo tempo_60 (1000000);

Mudela_score::Mudela_score (int format_i, int tracks_i, int tempo_i)
{
  format_i_ = format_i;
  tracks_i_ = tracks_i;
  tempo_i_ = tempo_i;
  column_l_array_.push (new Mudela_column (this, Moment (0)));
  //  mudela_key_l_ = &key_c;
  mudela_key_l_ = 0;
  mudela_time_signature_l_ = &time_sig_4;
  mudela_tempo_l_ = &tempo_60;
}

Mudela_score::~Mudela_score()
{
}

void
Mudela_score::add_item (Mudela_item* mudela_item_p)
{
  mudela_staff_p_list_.bottom()->add_item (mudela_item_p);
}

void
Mudela_score::add_staff (Mudela_staff* mudela_staff_p)
{
  mudela_staff_p_list_.bottom().add (mudela_staff_p);
}

Mudela_column*
Mudela_score::find_column_l (Moment mom)
{
#if 0
  // should do binary search
  for (int i = 0; i < column_l_array_.size (); i++ )
    if ( column_l_array_[i]->at_mom () == mom )
      return column_l_array_[i];
  return 0;
#else
  int upper_i = max (0, column_l_array_.size () - 1);
  int lower_i = 0;
  int i = 0; //upper_i;
  while (1)
    {
      Moment i_mom = column_l_array_ [i]->at_mom ();
      if (i_mom == mom)
	return column_l_array_ [i];
      if (mom < i_mom)
	upper_i = i;
      else
	lower_i = i;
      if ((upper_i == lower_i) || (i == column_l_array_.size () - 1))
	{
	  // we don't do inserts
	  assert (0);
	  Mudela_column* col_p = new Mudela_column (this, mom);
	  column_l_array_.push (col_p);
	  return col_p;
        }
      i = (upper_i + lower_i + 1 ) / 2;
    }
  assert (0);
  return 0;
#endif
}

Mudela_column*
Mudela_score::get_column_l (Moment mom)
{
  int i;
  Mudela_column *c=0;
  for (i=column_l_array_.size() - 1; !c && i >=0; i--)
    {
      if (column_l_array_ [i]->at_mom () == mom )
	c = column_l_array_[i];
      else if (column_l_array_[i]->at_mom () < mom)
	break;
    }
  if (!c)
    {
      c = new Mudela_column (this, mom);
      column_l_array_.insert (c, i+1);
    }

  assert (c->at_mom () == mom);
  return c;
}


void
Mudela_score::output (String filename_str)
{
  LOGOUT(NORMAL_ver) << _f ("Lily output to %s...", filename_str) << endl;

  // ugh, ugly midi type 1 fix
  if  ( (mudela_staff_p_list_.size() == 1) && !mudela_staff_p_list_.top()->number_i_)
    mudela_staff_p_list_.top()->number_i_ = 1;

  int track_i = 0;
  Mudela_stream mudela_stream (filename_str);
  for  (PCursor<Mudela_staff*> i (mudela_staff_p_list_); i.ok(); i++)
    {
      LOGOUT(NORMAL_ver) << _ ("track ") << track_i++ << ": " << flush;
      i->output (mudela_stream);
      mudela_stream << '\n';
      LOGOUT(NORMAL_ver) << endl;
    }

  mudela_stream << "\\score{\n";
  if  (mudela_staff_p_list_.size() > 1)
    mudela_stream << "< \n";
  for  (PCursor<Mudela_staff*> i (mudela_staff_p_list_); i.ok(); i++)
    {
      if  ( (mudela_staff_p_list_.size() != 1)
	    &&  (i == mudela_staff_p_list_.top()))
	continue;
      mudela_stream << "\\type Staff = \"" << i->id_str() << "\" ";
      mudela_stream << String ("\\" +  i->id_str ()) << "\n";
    }
  if  (mudela_staff_p_list_.size() > 1)
    mudela_stream << ">\n";


  mudela_stream << "\\paper{}\n";

  mudela_stream << "\\midi{\n";
  // let's not use silly 0 track
  mudela_staff_p_list_.bottom()->mudela_tempo_l_->output (mudela_stream);
  mudela_stream << "}\n";

  mudela_stream << "}\n";
}

void
Mudela_score::process()
{
  LOGOUT(NORMAL_ver) << '\n' << _ ("Processing...") << endl;

  LOGOUT(DEBUG_ver) << "columns\n";
  //  for  (PCursor<Mudela_column*> i (mudela_column_p_list_); i.ok(); i++)
  //	LOGOUT(DEBUG_ver) << "At: " << i->at_mom() << '\n';

  settle_columns();
  filter_tempo();
  quantify_columns();
  quantify_durations();

  LOGOUT(NORMAL_ver) << '\n' << _ ("Creating voices...") << endl;
  int track_i = 0;
  for  (PCursor<Mudela_staff*> i (mudela_staff_p_list_); i.ok(); i++)
    {
      LOGOUT(NORMAL_ver) << _ ("track ") << track_i++ << ": " << flush;
      i->process();
      LOGOUT(NORMAL_ver) << endl;
    }
}

void
Mudela_score::filter_tempo()
{
  LOGOUT(NORMAL_ver) << '\n' << _ ("NOT Filtering tempo...") << endl;
}

void
Mudela_score::quantify_columns()
{
  // ugh
  if  (Duration_convert::no_quantify_b_s)
    {
      LOGOUT(NORMAL_ver) << '\n' << _("NOT Quantifying columns...") << endl;
      return;
    }

  LOGOUT(NORMAL_ver) << '\n' << _("Quantifying columns...") << endl;

  int current_bar_i = 0;
  Moment bar_mom = mudela_time_signature_l_->bar_mom();

  int n = 5 >? Duration_convert::no_smaller_than_i_s;
  n = Duration_convert::type2_i (n);
  Moment s = Moment (1, n);
  Moment sh = Moment (1, 2 * n);
  for  (int i = 0; i < column_l_array_.size(); i++)
    {
      column_l_array_ [i]->at_mom_ =
      	s * Moment( (int) ( (column_l_array_ [i]->at_mom()) / s));

      int bar_i = (int) (column_l_array_ [i]->at_mom () / bar_mom) + 1;
      if (bar_i > current_bar_i)
	{
	  LOGOUT (NORMAL_ver) << "[" << bar_i << "]" << flush;
	  current_bar_i = bar_i;
	}
    }
  LOGOUT(NORMAL_ver) << endl;
}

void
Mudela_score::quantify_durations()
{
  //    LOGOUT(NORMAL_ver) << '\n' << "Quantifying durations..." << endl;
}

void
Mudela_score::settle_columns()
{
  //    LOGOUT(NORMAL_ver) << '\n' << "NOT Settling columns..." << endl;
  //    return;
  LOGOUT(NORMAL_ver) << '\n' << _("Settling columns...") << endl;

#if 0
  assert (!column_l_array_.size());
  int n = mudela_column_p_list_.size();
  // huh?
  //    column_l_array_.set_size (n);
  for  (PCursor<Mudela_column*> i (mudela_column_p_list_); i.ok(); i++)
    column_l_array_.push (*i);
#endif

  int n = column_l_array_.size();

  int start_i = 0;
  int end_i = 0;
  Moment start_mom = 0;
  Duration smallest_dur;
  smallest_dur.durlog_i_ =  6;
  Moment const noise_mom = Duration_convert::dur2_mom (smallest_dur)
    / Moment (2);
  for  (int i = 0; i < n; i++)
    {
      if  (!start_i)
	{
	  start_i = end_i = i;
	  start_mom = column_l_array_ [i]->at_mom();
	  continue;
	}

      // find all columns within noise's distance
      while  ( (i < n)
	       &&  (column_l_array_ [i]->at_mom() - start_mom < noise_mom))
	end_i = ++i;

      // bluntly set all to time of first in group
      for  (int j = start_i; j < end_i; j++)
	column_l_array_ [j]->at_mom_ = start_mom;

      start_i = end_i = 0;
    }
}

