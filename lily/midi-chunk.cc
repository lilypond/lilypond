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

#include "midi-chunk.hh"

#include "midi-item.hh"
#include "std-string.hh"
#include "string-convert.hh"

using std::string;
using std::vector;

Midi_track::Midi_track (int number, bool port)
  : number_ (number)
{
  //                4D 54 72 6B     MTrk
  //                00 00 00 3B     chunk length (59)
  //        00      FF 58 04 04 02 18 08    time signature
  //        00      FF 51 03 07 A1 20       tempo

  // FF 59 02 sf mi  Key Signature
  //         sf = -7:  7 flats
  //         sf = -1:  1 flat
  //         sf = 0:  key of C
  //         sf = 1:  1 sharp
  //         sf = 7: 7 sharps
  //         mi = 0:  major key
  //         mi = 1:  minor key

  char const *data_str0 = ""
    //        "00" "ff58" "0404" "0218" "08"
    //  "00" "ff51" "0307" "a120"
    // why a key at all, in midi?
    // key: C
    //  "00" "ff59" "02" "00" "00"
    // key: F (scsii-menuetto)
    //                            "00" "ff59" "02" "ff" "00"
    ;

  string data_string;
  // only for format 0 (currently using format 1)?
  data_string += String_convert::hex2bin (data_str0);

  if (port)
    {
      const char num = static_cast<char> (number);
      const char out[5] = {0x00, '\xff', 0x21, 0x01, num};
      data_string += string (out, sizeof (out));
    }

  set ("MTrk", data_string, "");
}

void
Midi_track::push_back (int delta_ticks, Midi_item *midi)
{
  assert (delta_ticks >= 0);
  Midi_event *e = new Midi_event (delta_ticks, midi);
  events_.push_back (e);
}

void
Midi_track::add (int delta_ticks, Midi_item *midi)
{
  assert (delta_ticks >= 0);

  Midi_event *e = new Midi_event (delta_ticks, midi);

  // Insertion position for the new event in the track.
  vector<Midi_event *>::iterator position (events_.end ());
  if (delta_ticks == 0
      && (!dynamic_cast<Midi_note *> (midi)
          || dynamic_cast<Midi_note_off *> (midi)))
    {
      // If the new event occurs at the same time as the most recently added
      // one, and the event does not represent the start of a note, insert the
      // new event before all notes (if any) already starting at this time.
      // This is to force notes to be started only after all other events
      // (such as changes in instruments) which occur at the same time have
      // taken effect.
      while (position != events_.begin ())
        {
          vector<Midi_event *>::iterator previous (position - 1);
          if (!dynamic_cast<Midi_note *> ((*previous)->midi_)
              || dynamic_cast<Midi_note_off *> ((*previous)->midi_))
            {
              // Found an event that does not represent the start of a note.
              // Exit the loop to insert the new event in the track after this
              // event.
              break;
            }
          else if ((*previous)->delta_ticks_ != 0)
            {
              // Found the start of a new note with delta_ticks_ != 0.  Prepare
              // to insert the new event before this event, swapping the
              // delta_ticks_ fields of the events to keep the sequence of
              // deltas consistent.
              e->delta_ticks_ = (*previous)->delta_ticks_;
              (*previous)->delta_ticks_ = 0;
              position = previous;
              break;
            }
          // Otherwise, the event in the track is the start of a note occurring
          // at the same time as the new event: continue searching for the
          // insertion position.
          position = previous;
        }
    }
  events_.insert (position, e);
}

string
Midi_track::data_string () const
{
  string str = Midi_chunk::data_string ();

  for (vector<Midi_event *>::const_iterator i (events_.begin ());
       i != events_.end (); i++)
    {
      str += (*i)->to_string ();
    }
  return str;
}

Midi_track::~Midi_track ()
{
  for (auto *event : events_)
    delete event;
}

/****************************************************************
  event
*/
Midi_event::Midi_event (int delta_ticks, Midi_item *midi)
{
  delta_ticks_ = delta_ticks;
  midi_ = midi;
}

string
Midi_event::to_string () const
{
  string delta_string = int2midi_varint_string (delta_ticks_);
  string midi_string = midi_->to_string ();
  return delta_string + midi_string;
}
/****************************************************************
 header
*/

Midi_header::Midi_header (int format, int tracks, int clocks_per_4)
{
  string str = String_convert::be_u16 (uint16_t (format))
               + String_convert::be_u16 (uint16_t (tracks))
               + String_convert::be_u16 (uint16_t (clocks_per_4));
  set ("MThd", str, "");
}

/****************************************************************
   chunk
 */
Midi_chunk::~Midi_chunk ()
{
}

void
Midi_chunk::set (const string &header_string, const string &data_string,
                 const string &footer_string)
{
  data_string_ = data_string;
  footer_string_ = footer_string;
  header_string_ = header_string;
}

string
Midi_chunk::data_string () const
{
  return data_string_;
}

string
Midi_chunk::to_string () const
{
  string str = header_string_;
  string dat = data_string ();
  uint32_t total = uint32_t (dat.length () + footer_string_.length ());
  str += String_convert::be_u32 (total);
  str += dat;
  str += footer_string_;

  return str;
}
