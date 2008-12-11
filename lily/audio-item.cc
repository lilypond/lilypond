/*
  audio-item.cc -- implement Audio items.

  source file of the GNU LilyPond music typesetter

  (c) 1997--2008 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "audio-item.hh"

#include "midi-item.hh"
#include "audio-column.hh"

Audio_instrument::Audio_instrument (string instrument_string)
{
  str_ = instrument_string;
}

void
Audio_item::render ()
{
}

Audio_column *
Audio_item::get_column () const
{
  return audio_column_;
}

Audio_item::Audio_item ()
{
  audio_column_ = 0;
}

Audio_note::Audio_note (Pitch p, Moment m, bool tie_event, Pitch transposing)
{
  pitch_ = p;
  length_mom_ = m;
  tied_ = 0;
  transposing_ = transposing;
  tie_event_ = tie_event;
}

void
Audio_note::tie_to (Audio_note *t)
{
  tied_ = t;
  Audio_note *first = t;
  while (first->tied_)
    first = first->tied_;
  first->length_mom_ += length_mom_;
  length_mom_ = 0;
}

Audio_key::Audio_key (int acc, bool major)
{
  accidentals_ = acc;
  major_ = major;
}

Audio_dynamic::Audio_dynamic ()
{
  volume_ = -1;
}

Audio_span_dynamic::Audio_span_dynamic ()
{
  grow_dir_ = CENTER;
}

void
Audio_span_dynamic::add_absolute (Audio_dynamic *d)
{
  assert (d);
  dynamics_.push_back (d);
}

Moment
remap_grace_duration (Moment m)
{
  return Moment (m.main_part_ + Rational (9,40) * m.grace_part_,
		 Rational (0));
}

Real
moment_to_real (Moment m)
{
  return remap_grace_duration (m).main_part_.to_double ();
}

int
moment_to_ticks (Moment m)
{
  return int (moment_to_real (m) * 384 * 4);
}

void
Audio_span_dynamic::render ()
{
  if (dynamics_.size () <= 1)
    return ;

  assert (dynamics_[0]->volume_ >= 0);

  while  (dynamics_.back ()->volume_ > 0
	  && dynamics_.size () > 1
	  && sign (dynamics_.back ()->volume_ - dynamics_[0]->volume_) != grow_dir_)
    {
      dynamics_.erase (dynamics_.end () - 1);
    }

  if (dynamics_.size () <= 1)
    {
      programming_error ("(de)crescendo on items with specified volume.");
      return ;
    }
  
  Real delta_v = grow_dir_ * 0.1;
  
  Real start_v = dynamics_[0]->volume_;
  if (dynamics_.back ()->volume_ < 0)
    dynamics_.back ()->volume_ = max (min (start_v + grow_dir_ * 0.25, 1.0), 0.1);

  delta_v = dynamics_.back ()->volume_ - dynamics_[0]->volume_;

  Moment start = dynamics_[0]->get_column ()->when ();

  Real total_t = moment_to_real (dynamics_.back ()->get_column ()->when () - start);
  
  for (vsize i = 1; i < dynamics_.size (); i ++)
    {
      Moment dt_moment = dynamics_[i]->get_column ()->when ()
	- start;

      Real dt =  moment_to_real (dt_moment);
      
      Real v = start_v + delta_v *  (dt / total_t);

      dynamics_[i]->volume_ = v;	
    }
}



Audio_tempo::Audio_tempo (int per_minute_4)
{
  per_minute_4_ = per_minute_4;
}

Audio_time_signature::Audio_time_signature (int beats, int one_beat)
{
  beats_ = beats;
  one_beat_ = one_beat;
}

Audio_text::Audio_text (Audio_text::Type type, string text_string)
{
  text_string_ = text_string;
  type_ = type;
}

