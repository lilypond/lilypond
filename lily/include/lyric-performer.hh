/*
  lyric-performer.hh -- declare Lyric_performer

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
           Jan Nieuwenhuizen <jan@digicash.com>
*/


#ifndef LYRIC_PERFOMER_HH
#define LYRIC_PERFOMER_HH

#include "lily-proto.hh"
#include "performer.hh"
#include "varray.hh"

class Lyric_performer : public Performer {
public:
    NAME_MEMBERS();
    Lyric_performer();
    ~Lyric_performer();

private:
    Array<Lyric_req*> lreq_arr_;
    virtual bool do_try_request(Request*);
    virtual void do_process_requests();
    virtual void do_post_move_processing();
};

#endif // LYRIC_PERFOMER_HH
