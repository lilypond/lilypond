/*
  midi-parser.hh -- declare Midi_parser

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Jan Nieuwenhuizen <jan@digicash.com>
*/


#ifndef MIDI_PARSER_HH
#define MIDI_PARSER_HH

// must, gcc 2.7.2{,.1} hits ico on midi-track-parser.cc:134 (@Midi_note)
#define INLINES

#ifdef INLINES

#define next_byte() (inline_next_byte (__FUNCTION__))
#define peek_byte() (inline_peek_byte (__FUNCTION__))
#define forward_byte_L(n) (inline_forward_byte_L (__FUNCTION__, n))

#else

#define next_byte()\
  ((info_l_->byte_L_ < info_l_->end_byte_L_ ?\
    *info_l_->byte_L_++\
   : (Byte const)exit (__FUNCTION__": unexpected EOF")));

#define peek_byte()\
  ((info_l_->byte_L_ < info_l_->end_byte_L_ ?\
    *info_l_->byte_L_\
  : (Byte const)exit (__FUNCTION__": unexpected EOF")));

#define forward_byte_L(n) (inline_forward_byte_L (__FUNCTION__, n))

#endif

#include "proto.hh"
#include "moment.hh"
#include "mi2mu-proto.hh"

struct Midi_parser_info
{
  Midi_parser_info();
  int division_1_i_;
  int format_i_;
  int tracks_i_;
  int errorlevel_i_;
  Byte const* byte_L_;
  Byte const* end_byte_L_;
  Source_file* source_l_;
  Mudela_score* score_l_;
  Moment bar_mom_;
};

#include "string.hh"

class Midi_parser
{
public:
  Midi_parser ();

  Midi_parser_info* info_l_;

protected:
//  Byte const* inline_forward_byte_L (int n)
  Byte const* inline_forward_byte_L (char const* fun, int n)
  {
    if (info_l_->byte_L_ + n < info_l_->end_byte_L_ )
      {
      Byte const* p = info_l_->byte_L_;
      info_l_->byte_L_ += n;
      return p;
    }
//    exit (__FUNCTION__": unexpected EOF");
    exit (String (fun) + ": unexpected EOF");
    return 0;
  }

#ifdef INLINES
//  Byte inline_next_byte () 
  Byte inline_next_byte (char const* fun) 
    {
      if (info_l_->byte_L_ < info_l_->end_byte_L_)
	 return *info_l_->byte_L_++;
//    exit (__FUNCTION__": unexpected EOF");
      exit (String (fun) + ": unexpected EOF");
      return 0;
    }

//  Byte inline_peek_byte ()
  Byte inline_peek_byte (char const* fun)
    {
      if (info_l_->byte_L_ < info_l_->end_byte_L_)
	return *info_l_->byte_L_;
//      exit (__FUNCTION__": unexpected EOF");
      exit (String (fun) + ": unexpected EOF");
      return 0;
    }
#endif

  int get_i (int);
  String get_str (int);
  unsigned get_u (int);
  int get_var_i ();
  int exit (String);
  void error (String);
  String message (String);
  void warning (String);
};

#endif // MIDI_PARSER_HH
