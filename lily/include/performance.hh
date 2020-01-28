/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2020 Jan Nieuwenhuizen <janneke@gnu.org>

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

#ifndef PERFORMANCE_HH
#define PERFORMANCE_HH

#include "music-output.hh"
#include "std-vector.hh"

/* MIDI output.  */
class Performance : public Music_output
{
public:
  Performance (bool ports = false);
  ~Performance ();
  OVERRIDE_CLASS_NAME (Performance);

  SCM get_header () const;
  void set_header (SCM header);

  void derived_mark () const override;

  void add_element (Audio_element *p);
  void process () override;
  void remap_grace_durations ();
  void output (Midi_stream &midi_stream,
               const std::string &performance_name) const;
  void output_header_track (Midi_stream &midi_stream) const;

  void print () const;
  void write_output (std::string filename,
                     const std::string &performance_name) const;

  std::vector<Audio_staff *> audio_staffs_;
  std::vector<Audio_element *> audio_elements_;
  Output_def *midi_;
  bool ports_;
  SCM header_;
};

#endif /* PERFORMANCE_HH */
