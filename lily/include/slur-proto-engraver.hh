/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2013--2015 Mike Solomon <mike@mikesolomon.org>

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

#ifndef SLUR_PROTO_ENGRAVER_HH
#define SLUR_PROTO_ENGRAVER_HH

#include "engraver.hh"
#include "moment.hh"
#include <map>

class Slur_proto_engraver : public Engraver
{
protected:
  Slur_proto_engraver (const char* double_property_name,
    const char* grob_name, const char* object_name, const char* event_name) :
      double_property_name_ (double_property_name),
      grob_name_ (grob_name), object_name_ (object_name),
      event_name_ (event_name) {}

  struct Event_info {
    Stream_event *slur_, *note_;
    Event_info (Stream_event *slur, Stream_event *note)
      : slur_ (slur), note_ (note)
    { }
  };
  // protected so that subclasses can see them
  vector<Event_info> start_events_;
  vector<Event_info> stop_events_;

  typedef std::multimap<Stream_event *, Spanner *> Note_slurs;
  Drul_array<Note_slurs> note_slurs_;
  vector<Grob *> slurs_;
  vector<Grob *> end_slurs_;
  vector<Grob_info> objects_to_acknowledge_;
  const char* double_property_name_;
  const char* grob_name_;
  const char* object_name_;
  const char* event_name_;
  virtual SCM event_symbol () = 0;

  DECLARE_ACKNOWLEDGER (inline_accidental);
  DECLARE_ACKNOWLEDGER (fingering);
  DECLARE_ACKNOWLEDGER (note_column);
  DECLARE_ACKNOWLEDGER (script);
  DECLARE_ACKNOWLEDGER (dots);
  DECLARE_ACKNOWLEDGER (text_script);
  DECLARE_END_ACKNOWLEDGER (tie);
  DECLARE_ACKNOWLEDGER (tuplet_number);

  void listen_note (Stream_event *ev);
  void listen_slur (Stream_event *ev, Stream_event *note = 0);
  void acknowledge_extra_object (Grob_info);
  void stop_translation_timestep ();
  void process_music ();

  bool can_create_slur (const string&, vsize, vsize *, Stream_event *);
  void create_slur (const string &spanner_id, Event_info evi, Grob *g_cause, Direction dir, bool left_broken);
  bool try_to_end (Event_info evi);

  virtual void set_melisma (bool);
  virtual void finalize ();
  virtual void derived_mark () const;

public:
  // no TRANSLATOR_DECLARATIONS (Slur_proto_engraver) needed since this
  // class is abstract
  DECLARE_TRANSLATOR_CALLBACKS (Slur_proto_engraver);
};

#endif // SLUR_PROTO_ENGRAVER_HH
