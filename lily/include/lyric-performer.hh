/*
  lyric-performer.hh -- declare Lyric_performer

  source file of the GNU LilyPond music typesetter

  (c) 1997 Jan Nieuwenhuizen <jan@digicash.com>
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

protected:
    void do_print() const;
    virtual bool do_try_request( Request* req_l );
    virtual void process_requests();

private:
    Array<Lyric_req*> lreq_arr_;
};

#endif // LYRIC_PERFOMER_HH
