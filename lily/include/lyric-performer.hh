/*
  lyric-performer.hh -- declare Lyric_performer

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Jan Nieuwenhuizen <janneke@gnu.org>
*/


#ifndef LYRIC_PERFOMER_HH
#define LYRIC_PERFOMER_HH

#include "lily-proto.hh"
#include "performer.hh"
#include "array.hh"

class Lyric_performer : public Performer {
public:
  VIRTUAL_COPY_CONS(Translator);
 Lyric_performer ();

protected:
  void do_print() const;
  virtual bool do_try_music (Music* req_l);
  virtual void do_process_requests();
  virtual void do_pre_move_processing ();

private:
  Link_array<Lyric_req> lreq_arr_;
  Audio_text* audio_p_;
};

#endif // LYRIC_PERFOMER_HH
