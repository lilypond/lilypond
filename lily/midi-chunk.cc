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

#include "midi-chunk.hh"

#include "midi-item.hh"
#include "std-string.hh"
#include "string-convert.hh"

Midi_track::Midi_track ()
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

  number_ = 0;

  char const *data_str0 = ""
    //        "00" "ff58" "0404" "0218" "08"
    //	"00" "ff51" "0307" "a120"
    // why a key at all, in midi?
    // key: C
    //	"00" "ff59" "02" "00" "00"
    // key: F (scsii-menuetto)
    //				  "00" "ff59" "02" "ff" "00"
    ;

  string data_string;
  // only for format 0 (currently using format 1)?
  data_string += String_convert::hex2bin (data_str0);

  char const *footer_str0 = "00" "ff2f" "00";
  string footer_string = String_convert::hex2bin (footer_str0);

  set ("MTrk", data_string, footer_string);
}

void
Midi_track::add (int delta_ticks, Midi_item *midi)
{
  assert (delta_ticks >= 0);

  Midi_event *e = new Midi_event (delta_ticks, midi);
  events_.push_back (e);
}

string
Midi_track::data_string () const
{
  string str = Midi_chunk::data_string ();

  for (vector<Midi_event*>::const_iterator i (events_.begin ());
       i != events_.end (); i ++)
    {
      str += (*i)->to_string ();
    }
  return str;
}


Midi_track::~Midi_track ()
{
  junk_pointers (events_); 
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
  assert (midi_string.length ());
  return delta_string + midi_string;
}
/****************************************************************
 header
*/ 

Midi_header::Midi_header (int format, int tracks, int clocks_per_4)
{
  string str;

  string format_string = String_convert::int2hex (format, 4, '0');
  str += String_convert::hex2bin (format_string);

  string tracks_string = String_convert::int2hex (tracks, 4, '0');
  str += String_convert::hex2bin (tracks_string);

  string tempo_string = String_convert::int2hex (clocks_per_4, 4, '0');
  str += String_convert::hex2bin (tempo_string);

  set ("MThd", str, "");
}


/****************************************************************
   chunk
 */
Midi_chunk::~Midi_chunk ()
{
  
}

void
Midi_chunk::set (string header_string, string data_string, string footer_string)
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
  string length_string = String_convert::int2hex (dat.length ()
						  + footer_string_.length (), 8, '0');
  length_string = String_convert::hex2bin (length_string);

  str += length_string;
  str += dat;
  str += footer_string_;

  return str;
}

