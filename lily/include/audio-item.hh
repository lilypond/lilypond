/*
  audio-item.hh -- declare Audio_items

  (c) 1996--2008 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef AUDIO_ITEM_HH
#define AUDIO_ITEM_HH

#include "audio-element.hh"
#include "moment.hh"
#include "pitch.hh"

class Audio_item : public Audio_element
{
public:
  Audio_item ();
  Audio_column *audio_column_;
  Audio_column *get_column () const;

  virtual void render ();
  
private:
  Audio_item (Audio_item const &);
  Audio_item &operator = (Audio_item const &);
};

class Audio_dynamic : public Audio_item
{
public:
  Audio_dynamic ();

  Real volume_;
};

class Audio_span_dynamic : public Audio_element
{
public:
  Direction grow_dir_;
  vector<Audio_dynamic*> dynamics_;


  virtual void render ();
  void add_absolute (Audio_dynamic*);
  Audio_span_dynamic ();
};


class Audio_key : public Audio_item
{
public:
  Audio_key (int acc, bool major);

  int accidentals_;
  bool major_;
};

class Audio_instrument : public Audio_item
{
public:
  Audio_instrument (string instrument_string);

  string str_;
};

class Audio_note : public Audio_item
{
public:
  Audio_note (Pitch p, Moment m, bool tie_event, Pitch transposition);

  void tie_to (Audio_note *);

  Pitch pitch_;
  Moment length_mom_;
  Pitch transposing_;
  
  Audio_note *tied_;
  bool tie_event_;
};

class Audio_piano_pedal : public Audio_item
{
public:
  string type_string_;
  Direction dir_;
};

class Audio_text : public Audio_item
{
public:
  enum Type
    {
      TEXT = 1, COPYRIGHT, TRACK_NAME, INSTRUMENT_NAME, LYRIC,
      MARKER, CUE_POINT
    };

  Audio_text (Audio_text::Type type, string text_string);

  Type type_;
  string text_string_;
};

class Audio_tempo : public Audio_item
{
public:
  Audio_tempo (int per_minute_4);

  int per_minute_4_;
};

class Audio_time_signature : public Audio_item
{
public:
  Audio_time_signature (int beats, int one_beat);

  int beats_;
  int one_beat_;
};

int moment_to_ticks (Moment);
Real moment_to_real (Moment);
Moment remap_grace_duration (Moment);

#endif // AUDIO_ITEM_HH

