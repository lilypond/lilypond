/*
  audio-column.hh -- declare Audio_column

  (c)  1997--1999 Jan Nieuwenhuizen <janneke@gnu.org>
 */

#ifndef AUDIO_COLUMN_HH
#define AUDIO_COLUMN_HH

#include "proto.hh"
#include "plist.hh"
#include "lily-proto.hh"
#include "moment.hh"
#include "pcursor.hh"


/**
    generic audio grouped vertically.
 */

class Audio_column { 
public:
    Audio_column (Moment at_mom);

    void add_audio_item (Audio_item* i_l);
    Moment at_mom() const;
    void print() const;

    Link_list<Audio_item *> audio_item_l_list_;
    Performance * performance_l_;

private:
    Audio_column (Audio_column const&);

    Moment at_mom_;
};


#endif // AUDIO_COLUMN_HH
