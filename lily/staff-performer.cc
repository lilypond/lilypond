/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Jan Nieuwenhuizen <janneke@gnu.org>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "audio-column.hh"
#include "audio-item.hh"
#include "audio-staff.hh"
#include "context.hh"
#include "international.hh"
#include "midi-cc-announcer.hh"
#include "performer-group.hh"
#include "warn.hh"
#include "lily-imports.hh"

#include "translator.icc"

#include <deque>
#include <map>

using std::deque;
using std::map;
using std::string;

/* Perform a staff. Individual notes should have their instrument
  (staff-wide) set, so we override play_element ()
*/
class Staff_performer : public Performer
{
public:
  TRANSLATOR_DECLARATIONS (Staff_performer);
  ~Staff_performer ();

protected:
  void acknowledge_audio_element (Audio_element_info info) override;
  void finalize () override;
  void initialize () override;
  void process_music ();
  void stop_translation_timestep ();

private:
  string new_instrument_string ();
  void set_instrument_name (const string &voice);
  void set_instrument (int channel, const string &voice);
  int get_channel (const string &instrument);
  Audio_staff *get_audio_staff (const string &voice);
  Audio_staff *new_audio_staff (const string &voice);

  class Midi_control_initializer : public Midi_control_change_announcer
  {
  public:
    Midi_control_initializer (Staff_performer *performer,
                              Audio_staff *audio_staff, int channel);

    SCM get_property_value (const char *property_name) override;
    void do_announce (Audio_control_change *item) override;

  private:
    Staff_performer *performer_;
    Audio_staff *audio_staff_;
    int channel_;
  };

  string instrument_string_;
  int channel_;
  Audio_instrument *instrument_;
  Audio_text *instrument_name_;
  Audio_text *name_;
  Audio_tempo *tempo_;
  map<string, Audio_staff *> staff_map_;
  map<string, int> channel_map_;
  // Would prefer to have the following two items be
  // members of the containing class Performance,
  // so they can be reset for each new midi file output.
  static map<string, int> static_channel_map_;
  static int channel_count_;
  // For now, ask the last Staff_performer clean up during its finalize method
  static int staff_performer_count_;
};

map<string, int> Staff_performer::static_channel_map_;
int Staff_performer::channel_count_ = 0;
int Staff_performer::staff_performer_count_ = 0;

void
Staff_performer::boot ()
{
}

ADD_TRANSLATOR (Staff_performer,
                /* doc */
                R"(

                )",

                /* create */
                R"(

                )",

                /* read */
                R"(
midiChannelMapping
midiMergeUnisons
midiSkipOffset
                )",

                /* write */
                R"(

                )");

Staff_performer::Staff_performer (Context *c)
  : Performer (c),
    channel_ (-1),
    instrument_ (0),
    instrument_name_ (0),
    name_ (0),
    tempo_ (0)
{
}

Staff_performer::~Staff_performer ()
{
}

void
Staff_performer::initialize ()
{
  ++staff_performer_count_;
}

Audio_staff *
Staff_performer::new_audio_staff (const string &voice)
{
  Audio_staff *audio_staff = new Audio_staff;
  audio_staff->merge_unisons_
    = from_scm<bool> (get_property (this, "midiMergeUnisons"));
  string track_name = context ()->id_string () + ":" + voice;
  if (track_name != ":")
    {
      name_ = new Audio_text (Audio_text::TRACK_NAME,
                              context ()->id_string () + ":" + voice);
      audio_staff->add_audio_item (name_);
      announce_element (Audio_element_info (name_, 0));
    }
  announce_element (Audio_element_info (audio_staff, 0));
  staff_map_[voice] = audio_staff;
  if (!instrument_string_.empty ())
    set_instrument (channel_, voice);
  // Set initial values (if any) for MIDI controls.
  Midi_control_initializer i (this, audio_staff, channel_);
  i.announce_control_changes ();
  return audio_staff;
}

Audio_staff *
Staff_performer::get_audio_staff (const string &voice)
{
  SCM channel_mapping = get_property (this, "midiChannelMapping");
  if (!scm_is_eq (channel_mapping, ly_symbol2scm ("instrument"))
      && staff_map_.size ())
    return staff_map_.begin ()->second;

  map<string, Audio_staff *>::const_iterator i = staff_map_.find (voice);
  if (i != staff_map_.end ())
    return i->second;
  map<string, Audio_staff *>::const_iterator e = staff_map_.find ("");
  if (staff_map_.size () == 1 && e != staff_map_.end ())
    {
      staff_map_[voice] = e->second;
      return e->second;
    }
  return new_audio_staff (voice);
}

void
Staff_performer::process_music ()
{
}

void
Staff_performer::set_instrument (int channel, const string &voice)
{
  instrument_ = new Audio_instrument (instrument_string_);
  instrument_->channel_ = channel;
  announce_element (Audio_element_info (instrument_, 0));
  Audio_staff *audio_staff = get_audio_staff (voice);
  audio_staff->add_audio_item (instrument_);
  SCM drums = Lily::percussion_p (ly_symbol2scm (instrument_string_));
  audio_staff->percussion_ = from_scm<bool> (drums);
}

void
Staff_performer::set_instrument_name (const string &voice)
{
  instrument_name_
    = new Audio_text (Audio_text::INSTRUMENT_NAME, instrument_string_);
  announce_element (Audio_element_info (instrument_name_, 0));
  get_audio_staff (voice)->add_audio_item (instrument_name_);
}

void
Staff_performer::stop_translation_timestep ()
{
  name_ = 0;
  tempo_ = 0;
  instrument_name_ = 0;
  instrument_ = 0;
}

void
Staff_performer::finalize ()
{
  Moment end_mom
    = now_mom () + from_scm (get_property (this, "midiSkipOffset"), Moment ());
  for (map<string, Audio_staff *>::iterator i = staff_map_.begin ();
       i != staff_map_.end (); ++i)
    {
      i->second->end_mom_ = end_mom;
    }

  staff_map_.clear ();
  channel_map_.clear ();
  if (staff_performer_count_)
    --staff_performer_count_;
  if (0 == staff_performer_count_)
    {
      static_channel_map_.clear ();
      channel_count_ = 0;
    }
}

string
Staff_performer::new_instrument_string ()
{
  // mustn't ask Score for instrument: it will return piano!
  SCM minstr = get_property (this, "midiInstrument");

  if (!scm_is_string (minstr) || ly_scm2string (minstr) == instrument_string_)
    return "";

  instrument_string_ = ly_scm2string (minstr);

  return instrument_string_;
}

int
Staff_performer::get_channel (const string &instrument)
{
  SCM channel_mapping = get_property (this, "midiChannelMapping");
  map<string, int> &channel_map
    = (!scm_is_eq (channel_mapping, ly_symbol2scm ("instrument")))
        ? channel_map_
        : static_channel_map_;

  if (scm_is_eq (channel_mapping, ly_symbol2scm ("staff")) && channel_ >= 0)
    return channel_;

  map<string, int>::const_iterator i = channel_map.find (instrument);
  if (i != channel_map.end ())
    return i->second;

  auto channel = (scm_is_eq (channel_mapping, ly_symbol2scm ("staff")))
                   ? channel_count_++
                   : static_cast<int> (channel_map.size ());

  /* MIDI players tend to ignore instrument settings on channel
     10, the percussion channel.  */
  if (channel % 16 == 9)
    {
      // TODO: Shouldn't this assign 9 rather than channel++?
      //
      // TODO: A hard-coded percussion entry ought to be created at the
      // beginning, otherwise an early lookup of the key might cause it to be
      // allocated an unexpected value.  Fixing this requires decoupling the
      // next channel number from the map size.
      //
      // TODO: Should this entry really be created for any case of channel
      // mapping, or perhaps only for the per-instrument case?
      channel_map["percussion"] = channel++;
      // TODO: Above, channel_count_ is incremented in the per-staff case only;
      // should that be considered here as well?
      channel_count_++;
    }

  if (channel > 15) // TODO: warn the first time only, maybe
    {
      warning (_ ("MIDI channel wrapped around"));
      warning (_ ("remapping modulo 16"));
      channel = channel % 16;
    }

  channel_map[instrument] = channel;
  return channel;
}

void
Staff_performer::acknowledge_audio_element (Audio_element_info inf)
{
  /* map each context (voice) to its own track */
  Context *c = inf.origin_contexts (this)[0];
  string voice;
  if (c->is_alias (ly_symbol2scm ("Voice")))
    voice = c->id_string ();
  SCM channel_mapping = get_property (this, "midiChannelMapping");
  string str = new_instrument_string ();
  if (!scm_is_eq (channel_mapping, ly_symbol2scm ("instrument")))
    channel_ = get_channel (voice);
  else if (channel_ < 0 && str.empty ())
    channel_ = get_channel (str);
  if (str.length ())
    {
      if (!scm_is_eq (channel_mapping, ly_symbol2scm ("voice")))
        channel_ = get_channel (str);
      set_instrument (channel_, voice);
      set_instrument_name (voice);
    }
  Audio_staff *audio_staff = get_audio_staff (voice);
  if (Audio_item *ai = dynamic_cast<Audio_item *> (inf.elem_))
    {
      ai->channel_ = channel_;
      audio_staff->add_audio_item (ai);
    }
}

Staff_performer::Midi_control_initializer::Midi_control_initializer (
  Staff_performer *performer, Audio_staff *audio_staff, int channel)
  : performer_ (performer),
    audio_staff_ (audio_staff),
    channel_ (channel)
{
}

SCM
Staff_performer::Midi_control_initializer::get_property_value (
  const char *property_name)
{
  return get_property (performer_, property_name);
}

void
Staff_performer::Midi_control_initializer::do_announce (
  Audio_control_change *item)
{
  item->channel_ = channel_;
  audio_staff_->add_audio_item (item);
  performer_->announce_element (Audio_element_info (item, 0));
}
