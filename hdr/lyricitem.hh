//
//  lyricitem.hh -- part of LilyPond
//
//  copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

#ifndef LYRIC_ITEM_HH
#define LYRIC_ITEM_HH

#include "textitem.hh"
  
struct Lyric_item : Text_item {
    /* *************** */
    Lyric_item(Lyric_req* lreq_l, int voice_count_i);
    virtual void do_pre_processing();    
};


#endif // LYRIC_ITEM_HH //

