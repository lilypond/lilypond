/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2007--2009 Han-Wen Nienhuys <hanwen@lilypond.org>

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
#include "std-vector.hh"


/**
   timed MIDI event
*/
class Midi_event
{
public:
  Midi_event (int delta, Midi_item *midi);

  int delta_ticks_;
  Midi_item *midi_;
  string to_string () const;
};




/**
   variable sized MIDI data
*/
class Midi_chunk
{
public:
  void set (string header_string, string data_string, string footer_string);
  virtual string to_string () const;
  virtual string data_string () const;
  DECLARE_CLASSNAME(Midi_chunk);
  virtual ~Midi_chunk ();
private:
  string data_string_;
  string footer_string_;
  string header_string_;
};

class Midi_header : public Midi_chunk
{
public:
  DECLARE_CLASSNAME(Midi_header);

  Midi_header (int format, int tracks, int clocks_per_4);
};

class Midi_track : public Midi_chunk
{
public:
  int number_;
  DECLARE_CLASSNAME(Midi_track);

  vector<Midi_event*> events_;

  Midi_track ();
  ~Midi_track ();

  void add (int, Midi_item *midi);
  virtual string data_string () const;
};

#endif /* MIDI_CHUNK_HH */

