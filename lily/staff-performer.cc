/*
  staff-performer.cc -- implement Staff_performer

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Jan Nieuwenhuizen <janneke@gnu.org>
 */

#include "staff-performer.hh"
#include "translator-group.hh"
#include "debug.hh"
#include "audio-column.hh"
#include "audio-item.hh"
#include "audio-staff.hh"

IMPLEMENT_IS_TYPE_B1 (Staff_performer,Performer_group_performer);
ADD_THIS_TRANSLATOR (Staff_performer);

Staff_performer::Staff_performer ()
{
  audio_staff_p_ = 0;
}

Staff_performer::~Staff_performer ()
{
  delete audio_staff_p_;
}

void
Staff_performer::do_creation_processing ()
{
  audio_staff_p_ = new Audio_staff;

  play (new Audio_text (Audio_text::TRACK_NAME, id_str_));

#if 1
  String str = new_instrument_str ();
  if (str.length_i ()) 
    // instrument description
    play (new Audio_text (Audio_text::INSTRUMENT_NAME, str));
#endif

  // tempo
  play (new Audio_tempo (get_tempo_i ()));

#if 1
  if (str.length_i ())
    // instrument
    play (new Audio_instrument (str));
#endif
   
  Performer_group_performer::do_creation_processing ();
}

void
Staff_performer::do_process_requests ()
{
  String str = new_instrument_str ();
  if (str.length_i ())
    {
      play (new Audio_text (Audio_text::INSTRUMENT_NAME, str));
      play (new Audio_instrument (str));
    }
  Performer_group_performer::do_process_requests ();
}


void
Staff_performer::do_removal_processing ()
{
  Performer_group_performer::do_removal_processing ();
  Performer::play (audio_staff_p_);
  audio_staff_p_ = 0;
}

String 
Staff_performer::new_instrument_str () 
{ 
  // mustn't ask Score for instrument: it will return piano!
  String str = get_property ("midi_instrument");
  if (!str.length_i ())
    str = get_property ("instrument");
  if (str == instrument_str_)
    return "";

  instrument_str_ = str;

  return instrument_str_;

/* ugh, but can 't
  if (properties_dict_.elem_b ("instrument"))
    return properties_dict_["instrument"];
  return "";
*/
}

void 
Staff_performer::play (Audio_element* p)
{
  if (p->is_type_b (Audio_item::static_name ())) 
    {
      audio_staff_p_->add_audio_item ( (Audio_item*)p);
    }
  Performer::play (p);
}

