/*
  staff-performer.cc -- implement Staff_performer

  source file of the GNU LilyPond music typesetter

  (c) 1997 Jan Nieuwenhuizen <jan@digicash.com>
 */

#include "staff-performer.hh"
#include "translator-group.hh"
#include "debug.hh"
#include "audio-column.hh"
#include "audio-item.hh"
#include "audio-staff.hh"

IMPLEMENT_IS_TYPE_B1(Staff_performer,Performer_group_performer);
ADD_THIS_TRANSLATOR(Staff_performer);

Staff_performer::Staff_performer()
{
  audio_staff_p_ = 0;
}

Staff_performer::~Staff_performer()
{
  delete audio_staff_p_;
}

void
Staff_performer::do_creation_processing()
{
  audio_staff_p_ = new Audio_staff;

  if (instrument_str().length_i()) 
    {
      // staff name
      play (new Audio_text (Audio_text::TRACK_NAME, id_str_));
      // instrument description
      play (new Audio_text (Audio_text::INSTRUMENT_NAME, instrument_str ()));
    }

  // tempo
  play (new Audio_tempo (get_tempo_i ()));

  if (instrument_str ().length_i ())
    // instrument
    play (new Audio_instrument (instrument_str ()));
  Performer_group_performer::do_creation_processing ();
}

void
Staff_performer::do_removal_processing()
{
  Performer_group_performer::do_removal_processing ();
  Performer::play (audio_staff_p_);
  audio_staff_p_ = 0;
}

String 
Staff_performer::instrument_str() 
{ 
  // mustn't ask Score for instrument: it will return piano!
  return get_property ("instrument");

/* ugh, but can't
  if (properties_dict_.elt_b ("instrument"))
    return properties_dict_["instrument"];
  return "";
*/
}

void 
Staff_performer::play (Audio_element* p)
{
  if (p->is_type_b (Audio_item::static_name())) 
    {
      audio_staff_p_->add ((Audio_item*)p);
    }
  Performer::play (p);
}

