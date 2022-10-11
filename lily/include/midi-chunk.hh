/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2007--2022 Han-Wen Nienhuys <hanwen@lilypond.org>

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

#ifndef MIDI_CHUNK_HH
#define MIDI_CHUNK_HH

#include "lily-proto.hh"
#include "virtual-methods.hh"

#include <string>
#include <vector>

/**
   timed MIDI event
*/
class Midi_event
{
public:
  Midi_event (int delta, Midi_item *midi);

  int delta_ticks_;
  Midi_item *midi_;
  std::string to_string () const;
};

/**
   variable sized MIDI data
*/
class Midi_chunk
{
public:
  void set (const std::string &header_string, const std::string &data_string,
            const std::string &footer_string);
  virtual std::string to_string () const;
  virtual std::string data_string () const;
  VIRTUAL_CLASS_NAME (Midi_chunk);
  virtual ~Midi_chunk ();

private:
  std::string data_string_;
  std::string footer_string_;
  std::string header_string_;
};

class Midi_header : public Midi_chunk
{
public:
  OVERRIDE_CLASS_NAME (Midi_header);

  Midi_header (int format, int tracks, int clocks_per_4);
};

class Midi_track : public Midi_chunk
{
public:
  int number_;
  int port_;
  OVERRIDE_CLASS_NAME (Midi_track);

  std::vector<Midi_event *> events_;

  Midi_track (int number, bool port);
  ~Midi_track ();

  void add (int, Midi_item *midi);
  std::string data_string () const override;
  void push_back (int, Midi_item *midi);
};

#endif /* MIDI_CHUNK_HH */
