/*
  lyric-performer.hh -- declare Lyric_performer

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Jan Nieuwenhuizen <janneke@gnu.org>
*/


#ifndef LYRIC_PERFOMER_HH
#define LYRIC_PERFOMER_HH

#include "lily-proto.hh"
#include "performer.hh"
#include "array.hh"

class Lyric_performer : public Performer {
public:
  VIRTUAL_COPY_CONS(Translator);
  

protected:
  void do_print() const;
  virtual bool do_try_music (Music* req_l);
  virtual void do_process_requests();

private:
  Array<Lyric_req*> lreq_arr_;
};

#endif // LYRIC_PERFOMER_HH
