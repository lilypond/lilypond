//
// lilypond-item.hh -- declare lilypond_item
//
// (c) 1997--2001 Jan Nieuwenhuizen <janneke@gnu.org>

#ifndef LILYPOND_ITEM_HH
#define LILYPOND_ITEM_HH

#include "midi2ly-proto.hh"
#include "string.hh"
#include "rational.hh"
#include "duration.hh"

// should these:
// * be Lilypond_items
// * be Voice_elements/requests
// * get a name-change
// ?

/// (lilypond_item)
class Lilypond_item 
{
public:
  Lilypond_item (Lilypond_column* lilypond_column_l);
  virtual ~Lilypond_item ();
    
  virtual Rational at_mom ();
  virtual Rational duration_mom ();
  void output (Lilypond_stream& lilypond_stream_r);
  virtual String str () = 0;

  Lilypond_column* lilypond_column_l_;
};

class Lilypond_key : public Lilypond_item 
{
public:
  Lilypond_key (int accidentals_i, int minor_i);

  String notename_str (int pitch_i);
  virtual String str ();

  //private:
  int accidentals_i_;
  int minor_i_;
};

class Lilypond_time_signature : public Lilypond_item 
{
public:
  Lilypond_time_signature (int num_i, int den_i, int division_4_i, int count_32_i);

  Duration i2_dur (int time_i, int division_1_i);
  int clocks_1_i ();
  int den_i ();
  int num_i ();
  virtual String str ();
  Rational bar_mom ();

private:
  Real sync_f_;
  Duration sync_dur_;
  int clocks_1_i_;
  int num_i_;
  int den_i_;
};

class Lilypond_note : public Lilypond_item 
{
public:
  Lilypond_note (Lilypond_column* lilypond_column_l, int channel_i, int pitch_i, int dyn_i);

  Duration duration ();
  virtual Rational duration_mom ();
  virtual String str ();
    
  //    int const c0_pitch_i_c_ = 60; // huh?
  static int const c0_pitch_i_c_ = 48;

  static bool const simple_plet_b_s = false;
  int channel_i_;
  int pitch_i_;
  Lilypond_column* end_column_l_;
};

class Lilypond_skip : public Lilypond_item 
{
public:
  Lilypond_skip (Lilypond_column* lilypond_column_l, Rational skip_mom);

  Duration duration ();
  virtual Rational duration_mom ();
  virtual String str ();

private:
  Rational mom_;
};


class Lilypond_tempo : public Lilypond_item 
{
public:
  Lilypond_tempo (int useconds_per_4_i);

  int get_tempo_i (Rational rational);
  virtual String str ();
  int useconds_per_4_i ();

private:
  int useconds_per_4_i_;
  Rational seconds_per_1_mom_;
};

class Lilypond_text : public Lilypond_item 
{
public:
  enum Type { 
    TEXT = 1, COPYRIGHT, TRACK_NAME, INSTRUMENT_NAME, LYRIC, 
    MARKER, CUE_POINT
  };
  Lilypond_text (Lilypond_text::Type type,  String str);
  virtual String str ();

  //private:
  Type type_;
  String text_str_;
};

#endif // LILYPOND_ITEM_HH

