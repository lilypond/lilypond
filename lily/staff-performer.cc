/*
  staff-performer.cc -- implement Staff_performer

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Jan Nieuwenhuizen <janneke@gnu.org>
 */

#include "staff-performer.hh"
#include "translator-group.hh"
#include "debug.hh"
#include "audio-column.hh"
#include "audio-item.hh"
#include "audio-staff.hh"


ADD_THIS_TRANSLATOR (Staff_performer);

Staff_performer::Staff_performer ()
{
  audio_staff_p_ = 0;
  instrument_p_ = 0;
  instrument_name_p_ = 0;
  name_p_ = 0;
  tempo_p_ = 0;
}

Staff_performer::~Staff_performer ()
{
}

void
Staff_performer::do_creation_processing ()
{
  audio_staff_p_ = new Audio_staff;
  announce_element (Audio_element_info (audio_staff_p_, 0));

  name_p_ = new Audio_text (Audio_text::TRACK_NAME, id_str_);
  announce_element (Audio_element_info (name_p_, 0));

  tempo_p_ = new Audio_tempo (get_tempo_i ());
  announce_element (Audio_element_info (tempo_p_, 0));

  Performer_group_performer::do_creation_processing ();
}

void
Staff_performer::do_process_music ()
{
  String str = new_instrument_str ();
  if (str.length_i ())
    {
      instrument_name_p_ = new Audio_text (Audio_text::INSTRUMENT_NAME, str);
      announce_element (Audio_element_info (instrument_name_p_, 0));
      instrument_p_ = new Audio_instrument (str);
      announce_element (Audio_element_info (instrument_p_, 0));
    }
  Performer_group_performer::do_process_music ();
}

void
Staff_performer::do_pre_move_processing ()
{
  if (name_p_)
    {
      play_element (name_p_);
      name_p_ = 0;
    }
  if (tempo_p_)
    {
      play_element (tempo_p_);
      tempo_p_ = 0;
    }
  if (instrument_name_p_)
    {
      play_element (instrument_name_p_);
      instrument_name_p_ = 0;
    }
  if (instrument_p_)
    {
      play_element (instrument_p_);
      instrument_p_ = 0;
    }
  Performer_group_performer::do_pre_move_processing ();
}

void
Staff_performer::do_removal_processing ()
{
  Performer_group_performer::do_removal_processing ();
  Performer::play_element (audio_staff_p_);
  audio_staff_p_ = 0;
}

String 
Staff_performer::new_instrument_str () 
{ 
  // mustn't ask Score for instrument: it will return piano!
  SCM minstr = get_property (ly_symbol2scm ("midiInstrument"));

  if (!gh_string_p(minstr))
    minstr = get_property (ly_symbol2scm ("instrument"));

  if (!gh_string_p (minstr)
      || ly_scm2string (minstr) == instrument_str_)
    return "";

  instrument_str_ = ly_scm2string (minstr);

  return instrument_str_;
}

void 
Staff_performer::play_element (Audio_element* p)
{
  if (Audio_item *ai = dynamic_cast<Audio_item *> (p)) 
    {
      audio_staff_p_->add_audio_item (ai);
    }
  Performer::play_element (p);
}

