/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2012 Jan Nieuwenhuizen <janneke@gnu.org>

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

#include <map>
#include <deque>

#include "audio-column.hh"
#include "audio-item.hh"
#include "audio-staff.hh"
#include "context.hh"
#include "international.hh"
#include "performer-group.hh"
#include "warn.hh"

/* Perform a staff. Individual notes should have their instrument
  (staff-wide) set, so we override play_element ()
*/
class Staff_performer : public Performer
{
public:
  TRANSLATOR_DECLARATIONS (Staff_performer);
  ~Staff_performer ();

protected:
  virtual void acknowledge_audio_element (Audio_element_info info);
  virtual void finalize ();
  virtual void initialize ();
  void process_music ();
  void stop_translation_timestep ();

private:
  string new_instrument_string ();
  void set_instrument_name (const string &voice);
  void set_instrument (int channel, const string &voice);
  int get_channel (const string &instrument);
  Audio_staff *get_audio_staff (const string &voice);
  Audio_staff *new_audio_staff (const string &voice);
  Audio_dynamic *get_dynamic (const string &voice);

  string instrument_string_;
  int channel_;
  Audio_instrument *instrument_;
  Audio_text *instrument_name_;
  Audio_text *name_;
  Audio_tempo *tempo_;
  map<string, deque<Audio_note *> > note_map_;
  map<string, Audio_staff *> staff_map_;
  map<string, int> channel_map_;
  map<string, Audio_dynamic *> dynamic_map_;
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

#include "translator.icc"

ADD_TRANSLATOR (Staff_performer,
                /* doc */
                "",

                /* create */
                "",

                /* read */
                "",

                /* write */
                "");

Staff_performer::Staff_performer ()
  : channel_ (-1),
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
    = to_boolean (get_property ("midiMergeUnisons"));
  string track_name = context ()->id_string () + ":" + voice;
  if (track_name != ":")
    {
      name_ = new Audio_text (Audio_text::TRACK_NAME, context ()->id_string ()
                              + ":" + voice);
      audio_staff->add_audio_item (name_);
      announce_element (Audio_element_info (name_, 0));
    }
  announce_element (Audio_element_info (audio_staff, 0));
  staff_map_[voice] = audio_staff;
  if (!instrument_string_.empty ())
    set_instrument (channel_, voice);
  // Set initial values (if any) for control functions.
  for (const Audio_control_function_value_change::Context_property *p
         = Audio_control_function_value_change::context_properties_;
       p->name_; ++p)
    {
      SCM value = get_property (p->name_);
      if (scm_is_number (value))
        {
          Real val = scm_to_double (value);
          if (val >= p->range_min_ && val <= p->range_max_)
            {
              // Normalize the value to the 0.0 to 1.0 range.
              val = ((val - p->range_min_)
                     / (p->range_max_ - p->range_min_));
              Audio_control_function_value_change *item
                = new Audio_control_function_value_change (p->control_, val);
              item->channel_ = channel_;
              audio_staff->add_audio_item (item);
              announce_element (Audio_element_info (item, 0));
            }
          else
            warning (_f ("ignoring out-of-range value change for MIDI "
                         "property `%s'",
                         p->name_));
        }
    }
  return audio_staff;
}

Audio_staff *
Staff_performer::get_audio_staff (const string &voice)
{
  SCM channel_mapping = get_property ("midiChannelMapping");
  if (channel_mapping != ly_symbol2scm ("instrument")
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

Audio_dynamic *
Staff_performer::get_dynamic (const string &voice)
{
  map<string, Audio_dynamic *>::const_iterator i = dynamic_map_.find (voice);
  if (i != dynamic_map_.end ())
    return i->second;
  return 0;
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
  SCM proc = ly_lily_module_constant ("percussion?");
  SCM drums = scm_call_1 (proc, ly_symbol2scm (instrument_string_.c_str ()));
  audio_staff->percussion_ = (drums == SCM_BOOL_T);
}

void
Staff_performer::set_instrument_name (const string &voice)
{
  instrument_name_ = new Audio_text (Audio_text::INSTRUMENT_NAME,
                                     instrument_string_);
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
  // For each voice with a note played in the current translation time step,
  // check if the voice has an Audio_dynamic registered: if yes, apply this
  // dynamic to every note played in the voice in the current translation time
  // step.
  for (map<string, deque<Audio_note *> >::iterator vi = note_map_.begin ();
       vi != note_map_.end (); ++vi)
    {
      Audio_dynamic *d = get_dynamic (vi->first);
      if (d)
        {
          for (deque<Audio_note *>::iterator ni = vi->second.begin ();
               ni != vi->second.end (); ++ni)
            (*ni)->dynamic_ = d;
        }
    }
  note_map_.clear ();
}

void
Staff_performer::finalize ()
{
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
  SCM minstr = get_property ("midiInstrument");

  if (!scm_is_string (minstr)
      || ly_scm2string (minstr) == instrument_string_)
    return "";

  instrument_string_ = ly_scm2string (minstr);

  return instrument_string_;
}

int
Staff_performer::get_channel (const string &instrument)
{
  SCM channel_mapping = get_property ("midiChannelMapping");
  map<string, int> &channel_map
    = (channel_mapping != ly_symbol2scm ("instrument"))
      ? channel_map_
      : static_channel_map_;

  if (channel_mapping == ly_symbol2scm ("staff")
      && channel_ >= 0)
    return channel_;

  map<string, int>::const_iterator i = channel_map.find (instrument);
  if (i != channel_map.end ())
    return i->second;

  int channel = (channel_mapping == ly_symbol2scm ("staff"))
                ? channel_count_++
                : channel_map.size ();

  /* MIDI players tend to ignore instrument settings on channel
     10, the percussion channel.  */
  if (channel % 16 == 9)
    {
      channel_map["percussion"] = channel++;
      channel_count_++;
    }

  if (channel > 15)
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
  if (Audio_item *ai = dynamic_cast<Audio_item *> (inf.elem_))
    {
      /* map each context (voice) to its own track */
      Context *c = inf.origin_contexts (this)[0];
      string voice;
      if (c->is_alias (ly_symbol2scm ("Voice")))
        voice = c->id_string ();
      SCM channel_mapping = get_property ("midiChannelMapping");
      string str = new_instrument_string ();
      if (channel_mapping != ly_symbol2scm ("instrument"))
        channel_ = get_channel (voice);
      else if (channel_ < 0 && str.empty ())
        channel_ = get_channel (str);
      if (str.length ())
        {
          if (channel_mapping != ly_symbol2scm ("voice"))
            channel_ = get_channel (str);
          set_instrument (channel_, voice);
          set_instrument_name (voice);
        }
      ai->channel_ = channel_;
      bool encode_dynamics_as_velocity_ = true;
      if (encode_dynamics_as_velocity_)
        {
          if (Audio_note *n = dynamic_cast<Audio_note *> (inf.elem_))
            {
              // Keep track of the notes played in the current voice in this
              // translation time step (for adjusting their dynamics later in
              // stop_translation_timestep).
              note_map_[voice].push_back (n);
            }
          else if (Audio_dynamic *d = dynamic_cast<Audio_dynamic *> (inf.elem_))
            {
              dynamic_map_[voice] = d;
              // Output volume as velocity: must disable Midi_dynamic output
              d->silent_ = true;
            }
        }
      Audio_staff *audio_staff = get_audio_staff (voice);
      audio_staff->add_audio_item (ai);
    }
}

