/*
  midi-track-parser.cc -- implement

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include <assert.h>
#include "string-convert.hh"
#include "mi2mu-global.hh"
#include "midi-track-parser.hh"
#include "mudela-column.hh"
#include "mudela-item.hh"
#include "mudela-score.hh"
#include "mudela-staff.hh"

Midi_track_parser::Midi_track_parser (Midi_parser_info* info_l, int i)
{
  info_l_ = info_l;
  at_mom_ = 0;
  track_info_p_ = 0;
  mudela_staff_p_ = new Mudela_staff (i, "", "", "");
  parse_header ();
  parse_delta_time ();
}

Midi_track_parser::~Midi_track_parser ()
{
  delete mudela_staff_p_;
  delete track_info_p_;
}

Moment
Midi_track_parser::at_mom ()
{
  return at_mom_;
}

bool
Midi_track_parser::eot ()
{
  if ( info_l_->byte_L_ < info_l_->end_byte_L_ )
    return false;
  return true;
}

void
Midi_track_parser::note_end (Mudela_column* col_l, int channel_i, int pitch_i, int aftertouch_i )
{
  // junk dynamics
  (void)aftertouch_i;

  assert (col_l);

  for (PCursor<Mudela_note*> i (open_note_l_list_.top ()); i.ok (); )
    {
      if ((i->pitch_i_ == pitch_i) && (i->channel_i_ == channel_i))
	{
	  i->end_column_l_ = col_l;
	  // LOGOUT(DEBUG_ver) << "Note: " << pitch_i;
	  // LOGOUT(DEBUG_ver) << "; " << i->mudela_column_l_->at_mom_;
	  // LOGOUT(DEBUG_ver) << ", " << i->end_column_l_->at_mom_ << '\n';
	  i.remove_p();
	  return;
	}
      else
	i++;
    }
  warning (_f ("junking note-end event: channel = %d, pitch = %d", 
	       channel_i, pitch_i));
}

void
Midi_track_parser::note_end_all (Mudela_column* col_l)
{
  // find
  assert (col_l);
  for (PCursor<Mudela_note*> i (open_note_l_list_.top ()); i.ok (); )
    {
      i->end_column_l_ = col_l;
      i.remove_p ();
    }
}

Mudela_staff*
Midi_track_parser::parse (Mudela_column* col_l)
{
  Moment mom = at_mom ();
  while (!eot () && (mom == at_mom ()))
    {
      Mudela_item* p = parse_event (col_l);
      if (p)
	mudela_staff_p_->add_item (p);
    }

  if (!eot())
    return 0;

  // catch-all
  note_end_all (col_l);

  Mudela_staff* p = mudela_staff_p_;
  mudela_staff_p_ = 0;
  return p;
}

void
Midi_track_parser::parse_delta_time ()
{
  if (eot ())
    return;
  int delta_i = get_var_i ();
  at_mom_ += Moment (delta_i, info_l_->division_1_i_);
}

Mudela_item*
Midi_track_parser::parse_event (Mudela_column* col_l)
{
  Byte byte = peek_byte ();
  // RUNNING_STATUS	[\x00-\x5f]
  if (byte <= 0x5f)
    {
      if (running_byte_ <= 0x5f)
	exit (_ ("invalid running status"));
      /*
	'running status' rather means 'missing status'.
	we'll just pretend we read the running status byte.
      */
      byte = running_byte_;
    }
  else
    byte = next_byte ();

  Mudela_item* item_p = 0;
  // DATA_ENTRY	[\x60-\x79]
  if ((byte >= 0x60) && (byte <= 0x79))
    {
      next_byte ();
    }
  // ALL_NOTES_OFF	[\x7a-\x7f]
  else if ((byte >= 0x7a) && (byte <= 0x7f))
    {
      next_byte ();
      next_byte ();
      note_end_all (col_l);
    }
  // NOTE_OFF	[\x80-\x8f]
  else if ((byte >= 0x80) && (byte <= 0x8f))
    {
      running_byte_ = byte;
      int channel_i = byte & ~0x90;
      int pitch_i = (int)next_byte ();
      int dyn_i = (int)next_byte ();
      note_end (col_l, channel_i, pitch_i, dyn_i);
    }
  // NOTE_ON		[\x90-\x9f]
  else if ((byte >= 0x90) && (byte <= 0x9f))
    {
      running_byte_ = byte;
      int channel_i = byte & ~0x90;
      int pitch_i = (int)next_byte ();
      int dyn_i = (int)next_byte ();
      /*
	sss: some broken devices encode NOTE_OFF as
	NOTE_ON with zero volume
      */
      if (dyn_i)
	{
	  Mudela_note* p = new Mudela_note (col_l, channel_i, pitch_i, dyn_i);
	  item_p = p;
	  open_note_l_list_.bottom ().add (p);
	}
      else
	{
	  note_end (col_l, channel_i, pitch_i, dyn_i);
	}
    }
  // POLYPHONIC_AFTERTOUCH	[\xa0-\xaf]
  else if ((byte >= 0xa0) && (byte <= 0xaf))
    {
      running_byte_ = byte;
      next_byte ();
      next_byte ();
    }
  // CONTROLMODE_CHANGE	[\xb0-\xbf]
  else if ((byte >= 0xb0) && (byte <= 0xbf))
    {
      running_byte_ = byte;
      next_byte ();
      next_byte ();
    }
  // PROGRAM_CHANGE	[\xc0-\xcf]
  else if ((byte >= 0xc0) && (byte <= 0xcf))
    {
      running_byte_ = byte;
      next_byte ();
    }
  // CHANNEL_AFTERTOUCH	[\xd0-\xdf]
  else if ((byte >= 0xd0) && (byte <= 0xdf))
    {
      running_byte_ = byte;
      next_byte ();
      next_byte ();
    }
  // PITCHWHEEL_RANGE	[\xe0-\xef]
  else if ((byte >= 0xe0) && (byte <= 0xef))
    {
      running_byte_ = byte;
      next_byte ();
      next_byte ();
    }
  // SYSEX_EVENT1	[\xf0]
  else if (byte == 0xf0)
    {
      int length_i = get_var_i ();
      String str = get_str (length_i);
    }
  // SYSEX_EVENT2	[\xf7]
  else if (byte == 0xf7)
    {
      int length_i = get_var_i ();
      String str = get_str (length_i);
    }
  // META_EVENT	[\xff]
  else if (byte == 0xff)
    {
      // SEQUENCE	[\x00][\x02]
      byte = next_byte ();
      if (byte == 0)
	{
	  next_byte ();
	  get_i (2);
	}
      // YYTEXT		[\x01]
      // YYCOPYRIGHT	[\x02]
      // YYTRACK_NAME	[\x03]
      // YYINSTRUMENT_NAME	[\x04]
      // YYLYRIC		[\x05]
      // YYMARKER		[\x06]
      // YYCUE_POINT	[\x07]
      else if ((byte >= 0x01) && (byte <= 0x07))
	{
	  // LOGOUT (DEBUG_ver) << "\n% Text(" << (int)byte << "):" << flush;
	  int length_i = get_var_i ();
	  String str = get_str (length_i);
	  // LOGOUT (DEBUG_ver) << str << endl;
	  Mudela_text::Type t = (Mudela_text::Type)byte;
	  Mudela_text* p = new Mudela_text (t, str);
	  item_p = p;
	  if (t == Mudela_text::COPYRIGHT)
	    mudela_staff_p_->copyright_str_ = p->text_str_;
	  else if (t == Mudela_text::TRACK_NAME)
	    mudela_staff_p_->name_str_ = p->text_str_;
	  else if (t == Mudela_text::INSTRUMENT_NAME)
	    mudela_staff_p_->instrument_str_ = p->text_str_;
	}
      // END_OF_TRACK	[\x2f][\x00]
      else
	{
	  Byte next = peek_byte ();
	  if ((byte == 0x2f) && (next == 0x00))
	    {
	      next_byte ();
	      info_l_->byte_L_ = info_l_->end_byte_L_;
	    }
	  // TEMPO		[\x51][\x03]
	  else if ((byte == 0x51) && (next == 0x03))
	    {
	      next_byte ();
	      unsigned useconds_per_4_u = get_u (3);
	      // $$ = new Mudela_tempo ( ($2 << 16) + ($3 << 8) + $4);
	      // LOGOUT (DEBUG_ver) << $$->str() << endl;
	      Mudela_tempo* p = new Mudela_tempo ( useconds_per_4_u );
	      item_p = p;
	      info_l_->score_l_->mudela_tempo_l_ = p;
	      mudela_staff_p_->mudela_tempo_l_ = p;
	    }
	  // SMPTE_OFFSET	[\x54][\x05]
	  else if ((byte == 0x54) && (next == 0x05))
	    {
	      next_byte ();
	      (int)next_byte ();
	      (int)next_byte ();
	      (int)next_byte ();
	      (int)next_byte ();
	      (int)next_byte ();
	    }
	  // TIME		[\x58][\x04]
	  else if ((byte == 0x58) && (next == 0x04))
	    {
	      next_byte ();
	      int num_i = (int)next_byte ();
	      int den_i = (int)next_byte ();
	      int clocks_4_i = (int)next_byte ();
	      int count_32_i = (int)next_byte ();
	      Mudela_time_signature* p = new Mudela_time_signature ( num_i, den_i, clocks_4_i, count_32_i );
	      item_p = p;
	      info_l_->score_l_->mudela_time_signature_l_ = p;
	      info_l_->bar_mom_ = p->bar_mom ();
	      mudela_staff_p_->mudela_time_signature_l_ = p;
	    }
	  // KEY		[\x59][\x02]
	  else if ((byte == 0x59) && (next == 0x02))
	    {
	      next_byte ();
	      int accidentals_i = (int)next_byte ();
	      int minor_i = (int)next_byte ();
	      Mudela_key* p = new Mudela_key (accidentals_i, minor_i);
	      item_p = p;
	      info_l_->score_l_->mudela_key_l_ = p;
	      mudela_staff_p_->mudela_key_l_ = p;
	    }
	  // SSME		[\0x7f][\x03]
	  else if ((byte == 0x7f) && (next == 0x03))
	    {
	      next_byte ();
	      int length_i = get_var_i ();
	      String str = get_str (length_i);
	      item_p = new Mudela_text ((Mudela_text::Type)byte, str);
	    }
	  else
	    {
	      next_byte ();
	      next_byte ();
	      warning (_ ("unimplemented MIDI meta-event"));
	    }
	}
    }
  else
    exit (_ ("invalid MIDI event"));

  if (item_p)
    item_p->mudela_column_l_ = col_l;

  parse_delta_time ();

  return item_p;
}

void
Midi_track_parser::parse_header ()
{
  String str = get_str (4);
  if ( str != "MTrk" )
    exit (_ ("MIDI track expected"));

  int length_i = get_i (4);
  // is this signed?
  if (length_i < 0)
    exit (_ ("invalid track length"));
  assert (!track_info_p_);
  track_info_p_ = new Midi_parser_info (*info_l_);
  track_info_p_->end_byte_L_ = track_info_p_->byte_L_ + length_i;
  forward_byte_L (length_i);
  //  forward_byte_L (length_i-1);
  info_l_ = track_info_p_;
}
