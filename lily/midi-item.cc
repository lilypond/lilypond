/*
  midi-item.cc -- implement Midi items.

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Jan Nieuwenhuizen <janneke@gnu.org>
 */

#include "warn.hh"
#include "main.hh"
#include "misc.hh"
#include "string.hh"
#include "string-convert.hh"
#include "midi-item.hh"
#include "midi-stream.hh"
#include "audio-item.hh"
#include "duration.hh"
#include "scm-option.hh"

#include "killing-cons.tcc"

Midi_item*
Midi_item::get_midi (Audio_item* a)
{
  if (Audio_key* i = dynamic_cast<Audio_key*> (a))
    return new Midi_key (i);
  else if (Audio_instrument* i = dynamic_cast<Audio_instrument*> (a))
    return i->str_.length () ? new Midi_instrument (i) : 0;
  else if (Audio_note* i = dynamic_cast<Audio_note*> (a))
    return new Midi_note (i);
  else if (Audio_dynamic* i = dynamic_cast<Audio_dynamic*> (a))
    return new Midi_dynamic (i);
  else if (Audio_piano_pedal* i = dynamic_cast<Audio_piano_pedal*> (a))
    return new Midi_piano_pedal (i);
  else if (Audio_tempo* i = dynamic_cast<Audio_tempo*> (a))
    return new Midi_tempo (i);
  else if (Audio_time_signature* i = dynamic_cast<Audio_time_signature*> (a))
    return new Midi_time_signature (i);
  else if (Audio_text* i = dynamic_cast<Audio_text*> (a))
    //return i->text_string_.length () ? new Midi_text (i) : 0;
    return new Midi_text (i);
  else
    assert (0);

  // isn't C++ grand?
  return 0;
}

void
Midi_chunk::set (String header_string, String data_string, String footer_string)
{
  data_string_ = data_string;
  footer_string_ = footer_string;
  header_string_ = header_string;
}
  
String
Midi_chunk::data_string () const
{
  return data_string_;
}

String
Midi_chunk::string () const
{
  String str = header_string_;
  String dat = data_string ();
  String length_string = String_convert::int2hex (dat.length () 
    + footer_string_.length (), 8, '0');
  length_string = String_convert::hex2bin (length_string);
  str += length_string;
  str += dat;
  str += footer_string_;
  return str;
}

Midi_duration::Midi_duration (Real seconds_f)
{
  seconds_ = seconds_f;
}

String
Midi_duration::string () const
{
  return String ("<duration: ") + to_string (seconds_) + ">";
}

Midi_event::Midi_event (Moment delta_mom, Midi_item* midi)
{
  delta_mom_ = delta_mom;
  midi_ = midi;
}

/*
  ugh. midi output badly broken since grace note hackage.
 */
String
Midi_event::string () const
{
  Rational rat_dt = (delta_mom_.main_part_ * Rational (384) +
    delta_mom_.grace_part_ * Rational (100))*Rational (4);
  int delta_i = int (rat_dt);

  String delta_string = Midi_item::i2varint_string (delta_i);
  String midi_string = midi_->string ();
  assert (midi_string.length ());
  return delta_string + midi_string;
}


Midi_header::Midi_header (int format_i, int tracks_i, int clocks_per_4_i)
{
  String str;
	
  String format_string = String_convert::int2hex (format_i, 4, '0');
  str += String_convert::hex2bin (format_string);
	
  String tracks_string = String_convert::int2hex (tracks_i, 4, '0');
  str += String_convert::hex2bin (tracks_string);

  String tempo_string = String_convert::int2hex (clocks_per_4_i, 4, '0');
  str += String_convert::hex2bin (tempo_string);

  set ("MThd", str, "");
}

Midi_instrument::Midi_instrument (Audio_instrument* a)
{
  audio_ = a;
  audio_->str_.to_lower ();
}

String
Midi_instrument::string () const
{
  Byte program_byte = 0;
  bool found = false;
  SCM proc = scm_primitive_eval (ly_symbol2scm ("midi-program")); 
  SCM program = gh_call1 (proc, ly_symbol2scm (audio_->str_.to_str0 ()));
  found = (program != SCM_BOOL_F);
  if (found)
    program_byte = gh_scm2int(program);
  else
      warning (_f ("no such instrument: `%s'", audio_->str_.to_str0 ()));

  String str = to_string ((char) (0xc0 + channel_)); //YIKES! FIXME: Should be track. -rz
  str += to_string ((char)program_byte);
  return str;
}

Midi_item::Midi_item ()
{
  channel_ = 0;
}

Midi_item::~Midi_item ()
{
}

String
Midi_item::i2varint_string (int i)
{
  int buffer_i = i & 0x7f;
  while ((i >>= 7) > 0) 
    {
      buffer_i <<= 8;
      buffer_i |= 0x80;
      buffer_i += (i & 0x7f);
    }

  String str;
  while (1) 
    {
      str += to_string ((char)buffer_i);
      if (buffer_i & 0x80)
	buffer_i >>= 8;
      else
	break;
    }
  return str;
}

Midi_key::Midi_key (Audio_key*a)
{
  audio_ = a;
}

String
Midi_key::string () const
{
  String str = "ff5902";
  str += String_convert::int2hex (audio_->accidentals_, 2, '0');
  if (audio_->major_)
    str += String_convert::int2hex (0, 2, '0');
  else
    str += String_convert::int2hex (1, 2, '0');
  return String_convert::hex2bin (str);
}

Midi_time_signature::Midi_time_signature (Audio_time_signature* a)
{
  audio_ = a;
  clocks_per_1_ = 18;
}

String
Midi_time_signature::string () const
{
  int num = audio_->beats_;
  int den = audio_->one_beat_;

  String str = "ff5804";
  str += String_convert::int2hex (num, 2, '0');
  str += String_convert::int2hex (intlog2 (den) , 2, '0');
  str += String_convert::int2hex (clocks_per_1_, 2, '0');
  str += String_convert::int2hex (8, 2, '0');
  return String_convert::hex2bin (str);
}

Midi_note::Midi_note (Audio_note* a)
{
  audio_ = a;
  dynamic_byte_ = 0x7f;
}

Moment
Midi_note::length_mom () const
{
  Moment m = audio_->length_mom_;
#if 0
  //junkme?
  if (m < Moment (Rational (1, 1000)))
    {
      warning (_ ("silly duration"));
      m = 1;
     }
#endif
  return m;
}

int
Midi_note::get_pitch () const
{
  int p = audio_->pitch_.semitone_pitch () + audio_->transposing_;
  if (p == INT_MAX)
    {
      warning (_ ("silly pitch"));
      p = 0;
     }
  return p;
}

String
Midi_note::string () const
{
  Byte status_byte = (char) (0x90 + channel_);

  String str = to_string ((char)status_byte);
  str += to_string ((char) (get_pitch () + c0_pitch_i_));

  str += to_string ((char)dynamic_byte_);
  return str;
}

Midi_note_off::Midi_note_off (Midi_note* n)
  : Midi_note (n->audio_)
{
  on_ = n;
  channel_ = n->channel_;

  // Anybody who hears any difference, or knows how this works?
  //  0 should definitely be avoided, notes stick on some sound cards.
  // 64 is supposed to be neutral
  
  aftertouch_byte_ = 64;
}

String
Midi_note_off::string () const
{
  Byte status_byte = (char) (0x80 + channel_);

  String str = to_string ((char)status_byte);
  str += to_string ((char) (get_pitch () + Midi_note::c0_pitch_i_));
  str += to_string ((char)aftertouch_byte_);
  return str;
}

Midi_dynamic::Midi_dynamic (Audio_dynamic* a)
{
  audio_ = a;
}

String
Midi_dynamic::string () const
{
  Byte status_byte = (char) (0xB0 + channel_);
  String str = to_string ((char)status_byte);

  /*
    Main volume controller (per channel):
    07 MSB
    27 LSB
   */
  static Real const full_scale = 127;
  
  int volume = (int) (audio_->volume_*full_scale);
  if (volume <= 0)
    volume = 1;
  if (volume > full_scale)
    volume = (int)full_scale;

  str += to_string ((char)0x07);
  str += to_string ((char)volume);
  return str;
}

Midi_piano_pedal::Midi_piano_pedal (Audio_piano_pedal* a)
{
  audio_ = a;
}

String
Midi_piano_pedal::string () const
{
  Byte status_byte = (char) (0xB0 + channel_);
  String str = to_string ((char)status_byte);

  if (audio_->type_string_ == "Sostenuto")
    str += to_string ((char)0x42);
  else if (audio_->type_string_ == "Sustain")
    str += to_string ((char)0x40);
  else if (audio_->type_string_ == "UnaCorda")
    str += to_string ((char)0x43);

  int pedal = ((1 - audio_->dir_) / 2) * 0x7f;
  str += to_string ((char)pedal);
  return str;
}

Midi_tempo::Midi_tempo (Audio_tempo* a)
{
  audio_ = a;
}

String
Midi_tempo::string () const
{
  int useconds_per_4_i = 60 * (int)1e6 / audio_->per_minute_4_;
  String str = "ff5103";
  str += String_convert::int2hex (useconds_per_4_i, 6, '0');
  return String_convert::hex2bin (str);
}

Midi_text::Midi_text (Audio_text* a)
{
  audio_ = a;
}

String
Midi_text::string () const
{
  String str = "ff" + String_convert::int2hex (audio_->type_, 2, '0');
  str = String_convert::hex2bin (str);
  str += i2varint_string (audio_->text_string_.length ());
  str += audio_->text_string_;
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

  number_ = 0;
	
  char const* data_str0 = ""
    //        "00" "ff58" "0404" "0218" "08"
    //	"00" "ff51" "0307" "a120"
    // why a key at all, in midi?
    // key: C
    //	"00" "ff59" "02" "00" "00"
    // key: F (scsii-menuetto)
    //				  "00" "ff59" "02" "ff" "00"
	;

  String data_string;
  // only for format 0 (currently using format 1)?
  data_string += String_convert::hex2bin (data_str0);

  char const* footer_str0 = "00" "ff2f" "00";
  String footer_string = String_convert::hex2bin (footer_str0);

  set ("MTrk", data_string, footer_string);
}

void 
Midi_track::add (Moment delta_time_mom, Midi_item* midi)
{
  assert (delta_time_mom >= Moment (0));

  Midi_event * e = new Midi_event (delta_time_mom, midi);
  event_p_list_.append (new Killing_cons<Midi_event> (e, 0));
}

String
Midi_track::data_string () const
{
  String str = Midi_chunk::data_string ();
  if (midi_debug_global_b)
    str += "\n";
  for (Cons<Midi_event> *i=event_p_list_.head_; i; i = i->next_) 
    {
      str += i->car_->string ();
      if (midi_debug_global_b)
        str += "\n";
    }
  return str;
}
