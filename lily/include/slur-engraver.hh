/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2013--2022 Mike Solomon <mike@mikesolomon.org>,
                2016 David Kastrup <dak@gnu.org>

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

#ifndef SLUR_ENGRAVER_HH
#define SLUR_ENGRAVER_HH

#include "engraver.hh"

#include <map>
#include <vector>

class Item;

class Slur_engraver : public Engraver
{
private:
  struct Event_info
  {
    Stream_event *slur_, *note_;
    Event_info (Stream_event *slur, Stream_event *note)
      : slur_ (slur),
        note_ (note)
    {
    }
  };

  std::vector<Event_info> start_events_;
  std::vector<Event_info> stop_events_;

  typedef std::multimap<Stream_event *, Spanner *> Note_slurs;
  Drul_array<Note_slurs> note_slurs_;
  std::vector<Spanner *> slurs_;
  std::vector<Spanner *> end_slurs_;

  // objects that we need for formatting, eg. scripts and ties.
  std::vector<Grob *> objects_to_acknowledge_;

protected:
  virtual SCM event_symbol () const;
  virtual bool double_property () const;
  virtual SCM grob_symbol () const;
  virtual const char *object_name () const;

  void acknowledge_note_column (Grob_info_t<Item>);
  void acknowledge_script (Grob_info);

  void listen_note (Stream_event *ev);
  // A slur on an in-chord note is not actually announced as an event
  // but rather produced by the note listener.
  void listen_note_slur (Stream_event *ev, Stream_event *note);
  void listen_slur (Stream_event *ev) { listen_note_slur (ev, 0); }
  void acknowledge_extra_object (Grob_info);
  void stop_translation_timestep ();
  void process_music ();

  bool can_create_slur (SCM, vsize, vsize *, Stream_event *);
  void create_slur (SCM spanner_id, Event_info evi, Grob *g_cause,
                    Direction dir, bool left_broken);
  bool try_to_end (Event_info evi);

  virtual void set_melisma (bool);
  void finalize () override;

public:
  TRANSLATOR_DECLARATIONS (Slur_engraver);
};

#endif // SLUR_ENGRAVER_HH
