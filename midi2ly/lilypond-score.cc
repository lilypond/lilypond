//
// lilypond-score.cc -- implement Lilypond_score
//
// copyright 1997 Jan Nieuwenhuizen <janneke@gnu.org>

#include <assert.h>
#include "rational.hh"
#include "duration.hh"
#include "duration-convert.hh"
#include "midi2ly-global.hh"
#include "lilypond-column.hh"
#include "lilypond-item.hh"
#include "lilypond-score.hh"
#include "lilypond-staff.hh"
#include "lilypond-stream.hh"

#include "killing-cons.tcc"

//static Lilypond_key key_c (0, 0);
static Lilypond_time_signature time_sig_4 (4, 2, 24, 8);
// useconds per 4: 250000 === 60 4 per minute
static Lilypond_tempo tempo_60 (1000000);

Lilypond_score::Lilypond_score (int format_i, int tracks_i, int tempo_i)
{
  last_staff_l_ =0;
  format_i_ = format_i;
  tracks_i_ = tracks_i;
  tempo_i_ = tempo_i;
  column_l_array_.push (new Lilypond_column (this, Rational (0)));
  //  lilypond_key_l_ = &key_c;
  lilypond_key_l_ = 0;
  lilypond_time_signature_l_ = &time_sig_4;
  lilypond_tempo_l_ = &tempo_60;
}

Lilypond_score::~Lilypond_score ()
{
}

void
Lilypond_score::add_item (Lilypond_item* lilypond_item_p)
{
  last_staff_l_->add_item (lilypond_item_p);
}

void
Lilypond_score::add_staff (Lilypond_staff* lilypond_staff_p)
{
  lilypond_staff_p_list_.append (new Killing_cons<Lilypond_staff> (lilypond_staff_p, 0));
  last_staff_l_ = lilypond_staff_p;
}

Lilypond_column*
Lilypond_score::find_column_l (Rational mom)
{
  int upper_i = max (0, column_l_array_.size () - 1);
  int lower_i = 0;
  int i = 0; //upper_i;
  while (1)
    {
      Rational i_mom = column_l_array_ [i]->at_mom ();
      if (i_mom == mom)
	return column_l_array_ [i];
      if (mom < i_mom)
	upper_i = i;
      else
	lower_i = i;
      if ( (upper_i == lower_i) || (i == column_l_array_.size () - 1))
	{
	  // we don't do inserts
	  assert (0);
	  Lilypond_column* col_p = new Lilypond_column (this, mom);
	  column_l_array_.push (col_p);
	  return col_p;
        }
      i = (upper_i + lower_i + 1 ) / 2;
    }
  assert (0);
  return 0;
}

Lilypond_column*
Lilypond_score::get_column_l (Rational mom)
{
  int i;
  Lilypond_column *c=0;
  for (i=column_l_array_.size () - 1; !c && i >=0; i--)
    {
      if (column_l_array_ [i]->at_mom () == mom )
	c = column_l_array_[i];
      else if (column_l_array_[i]->at_mom () < mom)
	break;
    }
  if (!c)
    {
      c = new Lilypond_column (this, mom);
      column_l_array_.insert (c, i+1);
    }

  assert (c->at_mom () == mom);
  return c;
}

void
Lilypond_score::output (String filename_str)
{
  LOGOUT (NORMAL_ver) << _f ("LY output to `%s'...", filename_str) << endl;
  
  // ugh, ugly midi type 1 fix
  if ( (lilypond_staff_p_list_.size_i () == 1)
       && !lilypond_staff_p_list_.head_->car_->number_i_)
    lilypond_staff_p_list_.head_->car_->number_i_ = 1;
  
  int track_i = 0;
  Lilypond_stream lilypond_stream (filename_str);
  for (Cons<Lilypond_staff>* i = lilypond_staff_p_list_.head_; i; i = i->next_)
    {
      LOGOUT (NORMAL_ver) << _f ("track %d:", track_i++) << flush;
      i->car_->output (lilypond_stream);
      lilypond_stream << '\n';
      LOGOUT (NORMAL_ver) << endl;
    }
  
  lilypond_stream << "\\score{\n";
  if (lilypond_staff_p_list_.size_i () > 1)
    lilypond_stream << "< \n";
  for (Cons<Lilypond_staff>* i = lilypond_staff_p_list_.head_; i; i = i->next_)
    {
      if ( (lilypond_staff_p_list_.size_i () != 1)
	   && (i->car_ == lilypond_staff_p_list_.head_->car_))
  	continue;
      lilypond_stream << "\\context Staff = \"" << i->car_->id_str () << "\" ";
      lilypond_stream << String ("\\" +  i->car_->id_str ()) << '\n';
    }
  if (lilypond_staff_p_list_.size_i () > 1)
    lilypond_stream << ">\n";
  
  lilypond_stream << "\\paper{}\n";
  
#if 1
  lilypond_stream << "\\midi{\n";
  
  // let's not use silly 0 track
  last_cons (lilypond_staff_p_list_.head_)->car_->lilypond_tempo_l_->output (lilypond_stream);
  lilypond_stream << "}\n";
#endif
  
  lilypond_stream << "}\n";
}
  
void
Lilypond_score::process ()
{
  LOGOUT (NORMAL_ver) << '\n' << _ ("Processing...") << endl;
  
  LOGOUT (DEBUG_ver) << "columns\n";
  
  settle_columns ();
  filter_tempo ();
  quantify_columns ();
  quantify_durations ();
  
  LOGOUT (NORMAL_ver) << '\n' << _ ("Creating voices...") << endl;
  int track_i = 0;
  for (Cons<Lilypond_staff>* i = lilypond_staff_p_list_.head_; i; i = i->next_)
    {
      LOGOUT (NORMAL_ver) << _ ("track ") << track_i++ << ": " << flush;
      i->car_->process ();
      LOGOUT (NORMAL_ver) << endl;
    }
}
  
void
Lilypond_score::filter_tempo ()
{
  LOGOUT (NORMAL_ver) << '\n' << _ ("NOT Filtering tempo...") << endl;
}
  
void
Lilypond_score::quantify_columns ()
{
  // ugh
  if (Duration_convert::no_quantify_b_s)
    {
      LOGOUT (NORMAL_ver) << '\n' << _ ("NOT Quantifying columns...") << endl;
      return;
    }
  
  LOGOUT (NORMAL_ver) << '\n' << _ ("Quantifying columns...") << endl;
  
  int current_bar_i = 0;
  Rational bar_mom = lilypond_time_signature_l_->bar_mom ();
  
  int n = 5 >? Duration_convert::no_smaller_than_i_s;
  n = Duration_convert::type2_i (n);
  Rational s = Rational (1, n);
  for (int i = 0; i < column_l_array_.size (); i++)
    {
      column_l_array_ [i]->at_mom_ =
      	s * Rational ( (int) ( (column_l_array_ [i]->at_mom ()) / s));
  
      int bar_i = (int) (column_l_array_ [i]->at_mom () / bar_mom) + 1;
      if (bar_i > current_bar_i)

  	{
  	  LOGOUT (NORMAL_ver) << "[" << bar_i << "]" << flush;
  	  current_bar_i = bar_i;
  	}
    }
  LOGOUT (NORMAL_ver) << endl;
}
  
void
Lilypond_score::quantify_durations ()
{
  
}
  
void
Lilypond_score::settle_columns ()
{
  LOGOUT (NORMAL_ver) << '\n' << _ ("Settling columns...") << endl;
  
  int n = column_l_array_.size ();
  
  int start_i = 0;
  int end_i = 0;
  Rational start_mom = 0;

  Duration smallest_dur;
  smallest_dur.durlog_i_ =  6;
  Rational const noise_mom = Duration_convert::dur2_mom (smallest_dur)
    / Rational (2);
  for (int i = 0; i < n; i++)
    {
      if (!start_i)
	{
	  start_i = end_i = i;
	  start_mom = column_l_array_ [i]->at_mom ();
	  continue;
	}

      // find all columns within noise's distance
      while ( (i < n)
	      && (column_l_array_ [i]->at_mom () - start_mom < noise_mom))
	end_i = ++i;

      // bluntly set all to time of first in group
      for (int j = start_i; j < end_i; j++)
	column_l_array_ [j]->at_mom_ = start_mom;

      start_i = end_i = 0;
    }
}

