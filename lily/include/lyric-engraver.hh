/*
  lyric-engraver.hh -- declare Lyric_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef LYRIC_ENGRAVER_HH
#define LYRIC_ENGRAVER_HH
#include "engraver.hh"
#include "varray.hh"

#include "lily-proto.hh"

class Lyric_engraver : public Request_engraver {
    Array<Lyric_req*> lreq_arr_;
    virtual bool do_try_request(Request*);
    virtual void do_process_requests();
    virtual void do_post_move_processing();
public:
    NAME_MEMBERS();
    Lyric_engraver();
};
#endif // LYRIC_ENGRAVER_HH
