/*
  audio-item.hh -- declare Audio_items

  (c) 1996--2000 Jan Nieuwenhuizen <janneke@gnu.org>
 */

#ifndef AUDIO_ITEM_HH
#define AUDIO_ITEM_HH

 #include <typeinfo>
#include "lily-proto.hh"
#include "string.hh"
#include "audio-element.hh"
#include "key-def.hh"
#include "musical-pitch.hh"
#include "moment.hh"
#include "drul-array.hh"

/**
  Any piece of audio information.
  We need virtual conclassors, 
  let's try decentralised factory for specific audio implemenations.

 */
class Audio_item : public Audio_element
{
public:
  Audio_item ();

  Audio_column* audio_column_l_;

protected:
  virtual void do_print () const;
  
private:
  Audio_item (Audio_item const&);
  Audio_item& operator=( Audio_item const&);
};

class Audio_dynamic : public Audio_item
{
public:
  Audio_dynamic (int volume);

  int volume_i_;
};

class Audio_key : public Audio_item
{
public:
  Audio_key (Key_def const& key);

  Key_def key_;
};

class Audio_instrument : public Audio_item
{
public:
  Audio_instrument (String instrument_str);

  String str_;
};
                                      
class Audio_note : public Audio_item
{
public:  
  Audio_note (Musical_pitch p, Moment m, int transposing_i = 0);

  void tie_to (Audio_note*);

  Musical_pitch pitch_;
  Moment length_mom_;
  Moment delayed_mom_;
  Moment delayed_until_mom_;
  int transposing_i_;
  Audio_note* tied_;
};

class Audio_text : public Audio_item
{
public:
  enum Type { 
    TEXT = 1, COPYRIGHT, TRACK_NAME, INSTRUMENT_NAME, LYRIC, 
    MARKER, CUE_POINT
  };
  
  Audio_text (Audio_text::Type type, String text_str);

  Type type_;
  String text_str_;
};

class Audio_tempo : public Audio_item
{
public:
  Audio_tempo (int per_minute_4_i);

  int per_minute_4_i_;
};

class Audio_tie : public Audio_item
{
public:
  Audio_tie ();
  void set_note (Direction, Audio_note*);
  Drul_array<Audio_note*> note_l_drul_;
};

class Audio_time_signature : public Audio_item
{
public:
  Audio_time_signature (int beats, int one_beat);

  int beats_i_;
  int one_beat_i_;
};

#endif // AUDIO_ITEM_HH

