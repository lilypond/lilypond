/*
  midi-item.cc -- implement Midi items.

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Jan Nieuwenhuizen <janneke@gnu.org>
 */

#include "proto.hh"
#include "plist.hh"
#include "pcursor.hh"
#include "debug.hh"
#include "misc.hh"
#include "string.hh"
#include "string-convert.hh"
#include "command-request.hh"
#include "musical-request.hh"
#include "midi-item.hh"
#include "midi-stream.hh"
#include "audio-item.hh"

IMPLEMENT_IS_TYPE_B (Midi_item);
IMPLEMENT_IS_TYPE_B1 (Midi_chunk, Midi_item);
IMPLEMENT_IS_TYPE_B1 (Midi_duration, Midi_item);
IMPLEMENT_IS_TYPE_B1 (Midi_header, Midi_chunk);
IMPLEMENT_IS_TYPE_B1 (Midi_instrument, Midi_item);
IMPLEMENT_IS_TYPE_B1 (Midi_key,Midi_item);
IMPLEMENT_IS_TYPE_B1 (Midi_time_signature, Midi_item);
IMPLEMENT_IS_TYPE_B1 (Midi_note, Midi_item);
IMPLEMENT_IS_TYPE_B1 (Midi_note_off, Midi_item);
IMPLEMENT_IS_TYPE_B1 (Midi_tempo, Midi_item);
IMPLEMENT_IS_TYPE_B1 (Midi_text, Midi_item);
IMPLEMENT_IS_TYPE_B1 (Midi_track, Midi_chunk);

Midi_chunk::Midi_chunk ()
  : Midi_item (0)
{
}

void
Midi_chunk::set (String header_str, String data_str, String footer_str)
{
  data_str_ = data_str;
  footer_str_ = footer_str;
  header_str_ = header_str;
}
  
String
Midi_chunk::data_str () const
{
  return data_str_;
}

String
Midi_chunk::str () const
{
  String str = header_str_;
  String dat = data_str ();
#if 1 
  String length_str = String_convert::i2hex_str (dat.length_i () 
#else
  // huh, huh??
  String length_str = String_convert::i2hex_str (data_str_.length_i () 
#endif
    + footer_str_.length_i (), 8, '0');
  length_str = String_convert::hex2bin_str (length_str);
  str += length_str;
  str += dat;
  str += footer_str_;
  return str;
}

Midi_duration::Midi_duration (Real seconds_f)
  : Midi_item (0)
{
  seconds_f_ = seconds_f;
}

String
Midi_duration::str () const
{
  return String ("<duration: ") + to_str (seconds_f_) + ">";
}

Midi_event::Midi_event (Moment delta_mom, Midi_item* mitem_p)
{
  delta_mom_ = delta_mom;
  mitem_p_ = mitem_p;
}

Midi_event::~Midi_event ()
{
// uhuh
//  delete mitem_p_;
}

String
Midi_event::str () const
{
  int delta_i = delta_mom_ * Moment (Duration::division_1_i_s);
  String delta_str = Midi_item::i2varint_str (delta_i);
  String mitem_str = mitem_p_->str ();
  assert (mitem_str.length_i ());
  return delta_str + mitem_str;
}


Midi_header::Midi_header (int format_i, int tracks_i, int clocks_per_4_i)
  : Midi_chunk ()
{
  String str;
	
  String format_str = String_convert::i2hex_str (format_i, 4, '0');
  str += String_convert::hex2bin_str (format_str);
	
  String tracks_str = String_convert::i2hex_str (tracks_i, 4, '0');
  str += String_convert::hex2bin_str (tracks_str);

  String tempo_str = String_convert::i2hex_str (clocks_per_4_i, 4, '0');
  str += String_convert::hex2bin_str (tempo_str);

  set ("MThd", str, "");
}

char const* const instrument_name_sz_a_[ ] = {

  /* default is usually piano */
  /* 0 "piano", */

	  /* (1-8 piano) */
	  /* 1 */ "acoustic grand",
	  /* 2 */ "bright acoustic",
	  /* 3 */ "electric grand",
	  /* 4 */ "honky-tonk",
	  /* 5 */ "electric piano 1",
	  /* 6 */ "electric piano 2",
	  /* 7 */ "harpsichord",
	  /* 8 */ "clav",

	  /* (9-16 chrom percussion) */
	  /* 9 */ "celesta",
	  /* 10 */ "glockenspiel",
	  /* 11 */ "music box",
	  /* 12 */ "vibraphone",
	  /* 13 */ "marimba",
	  /* 14 */ "xylophone",
	  /* 15 */ "tubular bells",
	  /* 16 */ "dulcimer",

	  /* (17-24 organ) */
	  /* 17 */ "drawbar organ",
	  /* 18 */ "percussive organ",
	  /* 19 */ "rock organ",
	  /* 20 */ "church organ",
	  /* 21 */ "reed organ",
	  /* 22 */ "accordion",
	  /* 23 */ "harmonica",
	  /* 24 */ "concertina",

	  /* (25-32 guitar) */
	  /* 25 */ "acoustic guitar (nylon)",
	  /* 26 */ "acoustic guitar (steel)",
	  /* 27 */ "electric guitar (jazz)",
	  /* 28 */ "electric guitar (clean)",
	  /* 29 */ "electric guitar (muted)",
	  /* 30 */ "overdriven guitar",
	  /* 31 */ "distorted guitar",
	  /* 32 */ "guitar harmonics",

	  /* (33-40 bass) */
	  /* 33 */ "acoustic bass",
	  /* 34 */ "electric bass (finger)",
	  /* 35 */ "electric bass (pick)",
	  /* 36 */ "fretless bass",
	  /* 37 */ "slap bass 1",
	  /* 38 */ "slap bass 2",
	  /* 39 */ "synth bass 1",
	  /* 40 */ "synth bass 2",

	  /* (41-48 strings) */
	  /* 41 */ "violin",
	  /* 42 */ "viola",
	  /* 43 */ "cello",
	  /* 44 */ "contrabass",
	  /* 45 */ "tremolo strings",
	  /* 46 */ "pizzicato strings",
	  /* 47 */ "orchestral strings",
	  /* 48 */ "timpani",

	  /* (49-56 ensemble) */
	  /* 49 */ "string ensemble 1",
	  /* 50 */ "string ensemble 2",
	  /* 51 */ "synthstrings 1",
	  /* 52 */ "synthstrings 2",
	  /* 53 */ "choir aahs",
	  /* 54 */ "voice oohs",
	  /* 55 */ "synth voice",
	  /* 56 */ "orchestra hit",

	  /* (57-64 brass) */
	  /* 57 */ "trumpet",
	  /* 58 */ "trombone",
	  /* 59 */ "tuba",
	  /* 60 */ "muted trumpet",
	  /* 61 */ "french horn",
	  /* 62 */ "brass section",
	  /* 63 */ "synthbrass 1",
	  /* 64 */ "synthbrass 2",

	  /* (65-72 reed) */
	  /* 65 */ "soprano sax",
	  /* 66 */ "alto sax",
	  /* 67 */ "tenor sax",
	  /* 68 */ "baritone sax",
	  /* 69 */ "oboe",
	  /* 70 */ "english horn",
	  /* 71 */ "bassoon",
	  /* 72 */ "clarinet",

	  /* (73-80 pipe) */
	  /* 73 */ "piccolo",
	  /* 74 */ "flute",
	  /* 75 */ "recorder",
	  /* 76 */ "pan flute",
	  /* 77 */ "blown bottle",
	  /* 78 */ "skakuhachi",
	  /* 79 */ "whistle",
	  /* 80 */ "ocarina",

	  /* (81-88 synth lead) */
	  /* 81 */ "lead 1 (square)",
	  /* 82 */ "lead 2 (sawtooth)",
	  /* 83 */ "lead 3 (calliope)",
	  /* 84 */ "lead 4 (chiff)",
	  /* 85 */ "lead 5 (charang)",
	  /* 86 */ "lead 6 (voice)",
	  /* 87 */ "lead 7 (fifths)",
	  /* 88 */ "lead 8 (bass+lead)",

	  /* (89-96 synth pad) */
	  /* 89 */ "pad 1 (new age)",
	  /* 90 */ "pad 2 (warm)",
	  /* 91 */ "pad 3 (polysynth)",
	  /* 92 */ "pad 4 (choir)",
	  /* 93 */ "pad 5 (bowed)",
	  /* 94 */ "pad 6 (metallic)",
	  /* 95 */ "pad 7 (halo)",
	  /* 96 */ "pad 8 (sweep)",

	  /* (97-104 synth effects) */
	  /* 97 */ "fx 1 (rain)",
	  /* 98 */ "fx 2 (soundtrack)",
	  /* 99 */ "fx 3 (crystal)",
	  /* 100 */ "fx 4 (atmosphere)",
	  /* 101 */ "fx 5 (brightness)",
	  /* 102 */ "fx 6 (goblins)",
	  /* 103 */ "fx 7 (echoes)",
	  /* 104 */ "fx 8 (sci-fi)",

	  /* (105-112 ethnic) */
	  /* 105 */ "sitar",
	  /* 106 */ "banjo",
	  /* 107 */ "shamisen",
	  /* 108 */ "koto",
	  /* 109 */ "kalimba",
	  /* 110 */ "bagpipe",
	  /* 111 */ "fiddle",
	  /* 112 */ "shanai",

	  /* (113-120 percussive) */
	  /* 113 */ "tinkle bell",
	  /* 114 */ "agogo",
	  /* 115 */ "steel drums",
	  /* 116 */ "woodblock",
	  /* 117 */ "taiko drum",
	  /* 118 */ "melodic tom",
	  /* 119 */ "synth drum",
	  /* 120 */ "reverse cymbal",

	  /* (121-128 sound effects) */
	  /* 121 */ "guitar fret noise",
	  /* 122 */ "breath noise",
	  /* 123 */ "seashore",
	  /* 124 */ "bird tweet",
	  /* 125 */ "telephone ring",
	  /* 126 */ "helicopter",
	  /* 127 */ "applause",
	  /* 128 */ "gunshot",
	  0
}; 

Midi_instrument::Midi_instrument (int channel_i, String instrument_str)
  : Midi_item (0)
{
  instrument_str_ = instrument_str;
  instrument_str_.to_lower ();
  channel_i_ = channel_i;
}

Midi_item::~Midi_item ()
{
}

String
Midi_instrument::str () const
{
  Byte program_byte = 0;
  for (int i = 0; instrument_name_sz_a_[i]; i++)
    if (instrument_str_ == String (instrument_name_sz_a_[ i ])) 
      {
	program_byte = (Byte)i;
	break;
      }

  String str = to_str ((char) (0xc0 + channel_i_));
  str += to_str ((char)program_byte);
  return str;
}

Midi_item::Midi_item (Audio_item* audio_item_l)
{
  audio_item_l_ = audio_item_l;
  channel_i_ = 0;
}

String
Midi_item::i2varint_str (int i)
{
  int buffer_i = i & 0x7f;
  while ( (i >>= 7) > 0) 
    {
      buffer_i <<= 8;
      buffer_i |= 0x80;
      buffer_i += (i & 0x7f);
    }

  String str;
  while (1) 
    {
      str += to_str ((char)buffer_i);
      if (buffer_i & 0x80)
	buffer_i >>= 8;
      else
	break;
    }
  return str;
}

Midi_key::Midi_key (Audio_item* audio_item_l)
  : Midi_item (audio_item_l)
{
}

String
Midi_key::str () const
{
  Key_change_req* k = audio_item_l_->req_l_->access_Command_req ()->access_Key_change_req ();
  int sharps_i = k->sharps_i ();
  int flats_i = k->flats_i ();

  // midi cannot handle non-conventional keys
  if (flats_i && sharps_i)
    {
      String str = _f ("unconventional key: flats: %d, sharps: %d", flats_i, 
        sharps_i);
      flats_i = sharps_i = 0;
    }
  int accidentals_i = sharps_i - flats_i;

  String str = "ff5902";
  str += String_convert::i2hex_str (accidentals_i, 2, '0');
  int minor_i = k->minor_b ();
  str += String_convert::i2hex_str (minor_i, 2, '0');
  return String_convert::hex2bin_str (str);
}

Midi_time_signature::Midi_time_signature (Audio_item* audio_item_l)
  : Midi_item (audio_item_l)
{
  clocks_per_1_i_ = 18;
}

String
Midi_time_signature::str () const
{
  Time_signature_change_req* m = audio_item_l_->req_l_->access_Command_req ()->access_Time_signature_change_req ();
  int num_i = m->beats_i_;
  int den_i = m->one_beat_i_;

  String str = "ff5804";
  str += String_convert::i2hex_str (num_i, 2, '0');
  str += String_convert::i2hex_str (intlog2 (den_i) , 2, '0');
  str += String_convert::i2hex_str (clocks_per_1_i_, 2, '0');
  str += String_convert::i2hex_str (8, 2, '0');
  return String_convert::hex2bin_str (str);
}

Midi_note::Midi_note (Audio_item* audio_item_l)
  : Midi_item (audio_item_l)
{
  dynamic_byte_ = 0x7f;
}

Moment
Midi_note::duration () const
{
  Moment m = audio_item_l_->req_l_->access_Musical_req ()->access_Rhythmic_req ()->duration ();
  if (m < Moment (1, 1000))
    {
      warning (_ ("silly duration"));
      m = 1;
     }
  return m;
}

int
Midi_note::pitch_i () const
{
  int p = audio_item_l_->req_l_->access_Musical_req ()->access_Melodic_req 
    ()->pitch_.semitone_pitch () 
    + ((Audio_note*)audio_item_l_)->transposing_i_;
  if (p == INT_MAX)
    {
      warning (_ ("silly pitch"));
      p = 0;
     }
  return p;
}

String
Midi_note::str () const
{
  Byte status_byte = (char) (0x90 + channel_i_);

  String str = to_str ((char)status_byte);
  str += to_str ((char) (pitch_i () + c0_pitch_i_c_));

  // poor man's staff dynamics:
  str += to_str ((char) (dynamic_byte_ - 0x10 * channel_i_));

  return str;
}

Midi_note_off::Midi_note_off (Midi_note* midi_note_l)
  : Midi_item (midi_note_l->audio_item_l_)
{
  // 0x64 is supposed to be neutral, but let's try
  aftertouch_byte_ = 0x64;
  channel_i_ = midi_note_l->channel_i_;
}

int
Midi_note_off::pitch_i () const
{
  return audio_item_l_->req_l_->access_Musical_req ()->access_Melodic_req 
    ()->pitch_.semitone_pitch ()
    + ((Audio_note*)audio_item_l_)->transposing_i_;
}

String
Midi_note_off::str () const
{
  Byte status_byte = (char) (0x80 + channel_i_);

  String str = to_str ((char)status_byte);
  str += to_str ((char) (pitch_i () + Midi_note::c0_pitch_i_c_));
  str += to_str ((char)aftertouch_byte_);
  return str;
}

Midi_tempo::Midi_tempo (Audio_item* audio_item_l)
  : Midi_item (audio_item_l)
{
  per_minute_4_i_ = ( (Audio_tempo*)audio_item_l_)->per_minute_4_i_;
}

Midi_tempo::Midi_tempo (int per_minute_4_i)
  : Midi_item (0)
{
  per_minute_4_i_ = per_minute_4_i;
}

String
Midi_tempo::str () const
{
  int useconds_per_4_i = 60 * (int)1e6 / per_minute_4_i_;
  String str = "ff5103";
  str += String_convert::i2hex_str (useconds_per_4_i, 6, '0');
  return String_convert::hex2bin_str (str);
}

Midi_text::Midi_text (Audio_item* audio_item_l)
  : Midi_item (audio_item_l)
{
  text_str_ = ( (Audio_text*)audio_item_l_)->text_str_;
  type_ = (Type) ( (Audio_text*)audio_item_l_)->type_;
}

Midi_text::Midi_text (Midi_text::Type type, String text_str)
  : Midi_item (0)
{
  text_str_ = text_str;
  type_ = type;
}

String
Midi_text::str () const
{
  String str = "ff" + String_convert::i2hex_str (type_, 2, '0');
  str = String_convert::hex2bin_str (str);
  str += i2varint_str (text_str_.length_i ());
  str += text_str_;
  return str;
}

Midi_track::Midi_track ()
  : Midi_chunk ()
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

  number_i_ = 0;
	
  char const* data_ch_C = ""
    //        "00" "ff58" "0404" "0218" "08"
    //	"00" "ff51" "0307" "a120"
    // why a key at all, in midi?
    // key: C
    //	"00" "ff59" "02" "00" "00"
    // key: F (scsii-menuetto)
    //				  "00" "ff59" "02" "ff" "00"
	;

  String data_str;
  // only for format 0 (currently using format 1)?
  data_str += String_convert::hex2bin_str (data_ch_C);

  char const* footer_ch_C = "00" "ff2f" "00";
  String footer_str = String_convert::hex2bin_str (footer_ch_C);

  set ("MTrk", data_str, footer_str);
}

void 
Midi_track::add (Moment delta_time_mom, Midi_item* mitem_p)
{
  assert (delta_time_mom >= Moment (0));

  event_p_list_.bottom ().add (new Midi_event (delta_time_mom, mitem_p));
}

String
Midi_track::data_str () const
{
  String str = Midi_chunk::data_str ();
  if (check_debug && !monitor->silent_b ("Midistrings"))
    str += "\n";
  for (PCursor<Midi_event*> i (event_p_list_); i.ok (); i++) 
    {
      str += i->str ();
      if (check_debug && !monitor->silent_b ("Midistrings"))
        str += "\n";
    }
  return str;
}

Midi_track::~Midi_track ()
{
}
