/*
  midi-score-parser.cc -- implement

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "moment.hh"
#include "source-file.hh"
#include "source.hh"
#include "mi2mu-global.hh"
#include "midi-score-parser.hh"
#include "midi-track-parser.hh"
#include "mudela-item.hh"
#include "mudela-score.hh"


void
Midi_score_parser::open (String filename_str, Sources* sources_l)
{
  info_l_->source_l_ = sources_l->get_file_l (filename_str);
  if (!info_l_->source_l_)
    ::error (_f ("can't find file: `%s\'", filename_str));
  info_l_->byte_L_ = (Byte const*)info_l_->source_l_->ch_C ();
  info_l_->end_byte_L_ = info_l_->byte_L_ + info_l_->source_l_->length_i () + 1;
}

Mudela_score*
Midi_score_parser::parse (String filename_str, Sources* sources_l)
{
  Midi_parser_info info;
  info_l_ = &info;
  open (filename_str, sources_l);
  parse_header ();
  return parse_score ();
}

void
Midi_score_parser::parse_header ()
{
  String str = get_str (4);
  if ( str != "MThd" )
    exit (_ ("MIDI header expected"));

  int length_i = get_i (4);
  // is this signed?
  if (length_i < 6)
    exit (_ ("Invalid header length"));
  info_l_->format_i_ = get_i (2);
  if (info_l_->format_i_ != 0 && info_l_->format_i_ != 1)
    exit (_("Invalid midi format"));
  info_l_->tracks_i_ = get_i (2);
  if (info_l_->tracks_i_ < 0 || info_l_->tracks_i_ > 32 )
    exit (_("Invalid number of tracks"));
  info_l_->division_1_i_ = get_i (2) * 4;
  if (info_l_->division_1_i_ < 0)
    exit (_f ("can't handle %s", _ ("non-metrical time")));
  // ugh
  Duration::division_1_i_s = info_l_->division_1_i_;
  forward_byte_L (length_i - 6);
}

int
Midi_score_parser::find_earliest_i (Link_array<Midi_track_parser>& tracks)
{
  int earliest_i = 0;
  Moment earliest_mom = infinity_mom;
  for (int i = 0; i < tracks.size(); i++)
    {
      if ( tracks [i]->at_mom () < earliest_mom )
	{
	  earliest_mom = tracks [i]->at_mom ();
	  earliest_i = i;
	}
    }
  return earliest_i;
}

Mudela_score*
Midi_score_parser::parse_score ()
{
  int current_bar_i = 0;
  Mudela_time_signature m4 (4, 2, 24, 8);
  Moment bar4_mom = m4.bar_mom ();

  Mudela_score* score_p = new Mudela_score( 1, 1, 1 );
  info_l_->score_l_ = score_p;

  Link_array<Midi_track_parser> tracks;
  for (int i = 0; i < info_l_->tracks_i_; i++)
    tracks.push (new Midi_track_parser (info_l_, i));

  LOGOUT (NORMAL_ver) << _ ("Parsing...\n");
  while (tracks.size ())
    {
      int i = find_earliest_i (tracks);
      Moment at_mom = tracks [i]->at_mom ();
      Mudela_column* column_l = score_p->get_column_l (at_mom);
      Mudela_staff* staff_p = tracks [i]->parse (column_l);
      if ( staff_p )
	{
	  score_p->add_staff (staff_p);
	  delete tracks [i];
	  tracks.del (i);
	}

      //  brr, musta have some progress
      for (int ii = 0; !info_l_->bar_mom_ && ii < tracks.size (); ii++)
	info_l_->bar_mom_ = tracks [ii]->info_l_->bar_mom_;

      int bar_i = (int) (at_mom
	    / (info_l_->bar_mom_ ? info_l_->bar_mom_ : bar4_mom)) + 1;
      if (bar_i > current_bar_i)
	{
	  LOGOUT (NORMAL_ver) << '[' << bar_i << ']' << flush;
	  current_bar_i = bar_i;
	}
    }
  return score_p;
}
