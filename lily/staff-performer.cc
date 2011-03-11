/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2011 Jan Nieuwenhuizen <janneke@gnu.org>

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

#include "warn.hh"
#include "audio-column.hh"
#include "audio-item.hh"
#include "audio-staff.hh"
#include "performer-group.hh"
#include "context.hh"

/* Perform a staff. Individual notes should have their instrument
  (staff-wide) set, so we override play_element ()
*/
class Staff_performer : public Performer
{
public:
  TRANSLATOR_DECLARATIONS (Staff_performer);
  ~Staff_performer ();

  string new_instrument_string ();
  string instrument_string_;

protected:
  virtual void acknowledge_audio_element (Audio_element_info info);
  virtual void finalize ();
  virtual void initialize ();
  void process_music ();
  void stop_translation_timestep ();
  void set_instrument_name ();
  void set_instrument (int channel);

private:
  Audio_staff *audio_staff_;
  Audio_instrument *instrument_;
  Audio_text *instrument_name_;
  Audio_text *name_;
  Audio_tempo *tempo_;
  map<string, int> channel_map_;
};

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
{
  audio_staff_ = 0;
  instrument_ = 0;
  instrument_name_ = 0;
  name_ = 0;
  tempo_ = 0;
}

Staff_performer::~Staff_performer ()
{
}

void
Staff_performer::initialize ()
{
  audio_staff_ = new Audio_staff;
  name_ = new Audio_text (Audio_text::TRACK_NAME, context ()->id_string ());

  audio_staff_->add_audio_item (name_);
  
  announce_element (Audio_element_info (audio_staff_, 0));
  announce_element (Audio_element_info (name_, 0));
}

void
Staff_performer::process_music ()
{
  string str = new_instrument_string ();
  if (str.length ())
    {
      set_instrument (0);
      set_instrument_name ();
    }
}

void
Staff_performer::set_instrument (int channel)
{
  instrument_ = new Audio_instrument (instrument_string_);
  instrument_->channel_ = channel;
  announce_element (Audio_element_info (instrument_, 0));
  audio_staff_->add_audio_item (instrument_);
}

void
Staff_performer::set_instrument_name ()
{
  instrument_name_ = new Audio_text (Audio_text::INSTRUMENT_NAME,
				     instrument_string_);
  announce_element (Audio_element_info (instrument_name_, 0));
  audio_staff_->add_audio_item (instrument_name_);
}

void
Staff_performer::stop_translation_timestep ()
{
  SCM proc = ly_lily_module_constant ("percussion?");
  SCM drums = scm_call_1 (proc, ly_symbol2scm (instrument_string_.c_str ()));
  audio_staff_->percussion_ = (drums == SCM_BOOL_T);

  name_ = 0;
  tempo_ = 0;
  instrument_name_ = 0;
  instrument_ = 0;
}

void
Staff_performer::finalize ()
{
  audio_staff_ = 0;
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

void
Staff_performer::acknowledge_audio_element (Audio_element_info inf)
{
  if (Audio_item *ai = dynamic_cast<Audio_item *> (inf.elem_))
    {
      /* map each context (voice) to its own channel */
      Context *c = inf.origin_contexts (this)[0];
      string id = c->id_string ();
      int channel = channel_map_.size ();
      /* MIDI players tend to ignore instrument settings on channel
	 10, the percussion channel.  */
      if (channel % 16 == 9)
	channel_map_[""] = channel++;

      map<string, int>::const_iterator i = channel_map_.find (id);
      if (i != channel_map_.end ())
	channel = i->second;
      else
	{
	  channel_map_[id] = channel;
	  if (channel)
	    set_instrument (channel);
	}

      ai->channel_ = channel;
      audio_staff_->add_audio_item (ai);
    }
}

