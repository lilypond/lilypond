/*
  midi-item.cc -- implement Midi items.

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Jan Nieuwenhuizen <janneke@gnu.org>
 */

#include "debug.hh"
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
Midi_item::midi_p (Audio_item* a)
{
  if (Audio_key* i = dynamic_cast<Audio_key*> (a))
    return new Midi_key (i);
  else if (Audio_instrument* i = dynamic_cast<Audio_instrument*> (a))
    return i->str_.length_i () ? new Midi_instrument (i) : 0;
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
    //return i->text_str_.length_i () ? new Midi_text (i) : 0;
    return new Midi_text (i);
  else
    assert (0);

  // isn't C++ grand?
  return 0;
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
  String length_str = String_convert::i2hex_str (dat.length_i () 
    + footer_str_.length_i (), 8, '0');
  length_str = String_convert::hex2bin_str (length_str);
  str += length_str;
  str += dat;
  str += footer_str_;
  return str;
}

Midi_duration::Midi_duration (Real seconds_f)
{
  seconds_f_ = seconds_f;
}

String
Midi_duration::str () const
{
  return String ("<duration: ") + to_str (seconds_f_) + ">";
}

Midi_event::Midi_event (Moment delta_mom, Midi_item* midi_p)
{
  delta_mom_ = delta_mom;
  midi_p_ = midi_p;
}

/*
  ugh. midi output badly broken since grace note hackage.
 */
String
Midi_event::str () const
{
  Rational rat_dt = (delta_mom_.main_part_ * Rational (384) +
    delta_mom_.grace_part_ * Rational (100))*Rational (4);
  int delta_i = int (rat_dt);

  String delta_str = Midi_item::i2varint_str (delta_i);
  String midi_str = midi_p_->str ();
  assert (midi_str.length_i ());
  return delta_str + midi_str;
}


Midi_header::Midi_header (int format_i, int tracks_i, int clocks_per_4_i)
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

Midi_instrument::Midi_instrument (Audio_instrument* a)
{
  audio_l_ = a;
  audio_l_->str_.to_lower ();
}

String
Midi_instrument::str() const
{
  Byte program_byte = 0;
  bool found = false;
  SCM proc = scm_primitive_eval (ly_symbol2scm ("midi-program")); 
  SCM program = gh_call1 (proc, ly_symbol2scm (audio_l_->str_.ch_C()));
  found = (program != SCM_BOOL_F);
  if (found)
    program_byte = gh_scm2int(program);
  else
      warning (_f ("no such instrument: `%s'", audio_l_->str_.ch_C ()));

  String str = to_str ((char) (0xc0 + channel_i_)); //YIKES! FIXME: Should be track. -rz
  str += to_str ((char)program_byte);
  return str;
}

Midi_item::Midi_item ()
{
  channel_i_ = 0;
}

Midi_item::~Midi_item ()
{
}

String
Midi_item::i2varint_str (int i)
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
      str += to_str ((char)buffer_i);
      if (buffer_i & 0x80)
	buffer_i >>= 8;
      else
	break;
    }
  return str;
}

Midi_key::Midi_key (Audio_key*a)
{
  audio_l_ = a;
}

String
Midi_key::str () const
{
  String str = "ff5902";
  str += String_convert::i2hex_str (audio_l_->accidentals_, 2, '0');
  if (audio_l_->major_)
    str += String_convert::i2hex_str (0, 2, '0');
  else
    str += String_convert::i2hex_str (1, 2, '0');
  return String_convert::hex2bin_str (str);
}

Midi_time_signature::Midi_time_signature (Audio_time_signature* a)
{
  audio_l_ = a;
  clocks_per_1_i_ = 18;
}

String
Midi_time_signature::str () const
{
  int num = audio_l_->beats_i_;
  int den = audio_l_->one_beat_i_;

  String str = "ff5804";
  str += String_convert::i2hex_str (num, 2, '0');
  str += String_convert::i2hex_str (intlog2 (den) , 2, '0');
  str += String_convert::i2hex_str (clocks_per_1_i_, 2, '0');
  str += String_convert::i2hex_str (8, 2, '0');
  return String_convert::hex2bin_str (str);
}

Midi_note::Midi_note (Audio_note* a)
{
  audio_l_ = a;
  dynamic_byte_ = 0x7f;
}

Moment
Midi_note::length_mom () const
{
  Moment m = audio_l_->length_mom_;
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
Midi_note::pitch_i () const
{
  int p = audio_l_->pitch_.semitone_pitch () + audio_l_->transposing_i_;
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

  str += to_str ((char)dynamic_byte_);
  return str;
}

Midi_note_off::Midi_note_off (Midi_note* n)
  : Midi_note (n->audio_l_)
{
  on_l_ = n;
  channel_i_ = n->channel_i_;

  // Anybody who hears any difference, or knows how this works?
  //  0 should definitely be avoided, notes stick on some sound cards.
  // 64 is supposed to be neutral
  
  aftertouch_byte_ = 64;
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

Midi_dynamic::Midi_dynamic (Audio_dynamic* a)
{
  audio_l_ = a;
}

String
Midi_dynamic::str () const
{
  Byte status_byte = (char) (0xB0 + channel_i_);
  String str = to_str ((char)status_byte);

  /*
    Main volume controller (per channel):
    07 MSB
    27 LSB
   */
  static Real const full_scale = 127;
  
  int volume = (int) (audio_l_->volume_*full_scale);
  if (volume <= 0)
    volume = 1;
  if (volume > full_scale)
    volume = (int)full_scale;

  str += to_str ((char)0x07);
  str += to_str ((char)volume);
  return str;
}

Midi_piano_pedal::Midi_piano_pedal (Audio_piano_pedal* a)
{
  audio_l_ = a;
}

String
Midi_piano_pedal::str () const
{
  Byte status_byte = (char) (0xB0 + channel_i_);
  String str = to_str ((char)status_byte);

  if (audio_l_->type_str_ == "Sostenuto")
    str += to_str ((char)0x42);
  else if (audio_l_->type_str_ == "Sustain")
    str += to_str ((char)0x40);
  else if (audio_l_->type_str_ == "UnaCorda")
    str += to_str ((char)0x43);

  int pedal = ((1 - audio_l_->dir_) / 2) * 0x7f;
  str += to_str ((char)pedal);
  return str;
}

Midi_tempo::Midi_tempo (Audio_tempo* a)
{
  audio_l_ = a;
}

String
Midi_tempo::str () const
{
  int useconds_per_4_i = 60 * (int)1e6 / audio_l_->per_minute_4_i_;
  String str = "ff5103";
  str += String_convert::i2hex_str (useconds_per_4_i, 6, '0');
  return String_convert::hex2bin_str (str);
}

Midi_text::Midi_text (Audio_text* a)
{
  audio_l_ = a;
}

String
Midi_text::str () const
{
  String str = "ff" + String_convert::i2hex_str (audio_l_->type_, 2, '0');
  str = String_convert::hex2bin_str (str);
  str += i2varint_str (audio_l_->text_str_.length_i ());
  str += audio_l_->text_str_;
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
Midi_track::add (Moment delta_time_mom, Midi_item* midi_p)
{
  assert (delta_time_mom >= Moment (0));

  Midi_event * e = new Midi_event (delta_time_mom, midi_p);
  event_p_list_.append (new Killing_cons<Midi_event> (e, 0));
}

String
Midi_track::data_str () const
{
  String str = Midi_chunk::data_str ();
  if (midi_debug_global_b)
    str += "\n";
  for (Cons<Midi_event> *i=event_p_list_.head_; i; i = i->next_) 
    {
      str += i->car_->str ();
      if (midi_debug_global_b)
        str += "\n";
    }
  return str;
}
