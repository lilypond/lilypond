/*
  tempo-performer.hh -- declare Tempo_performer

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef TEMPO_PERFORMER_HH
#define TEMPO_PERFORMER_HH

#include "lily-proto.hh"
#include "performer.hh"

class Tempo_performer : public Performer
{
public:
  VIRTUAL_COPY_CONS(Translator);
  
  Tempo_performer();
  ~Tempo_performer();

protected:
  void do_print() const;
  virtual bool do_try_music (Music* req_l);
  virtual void do_process_requests();
  virtual void do_pre_move_processing ();

private:
  Tempo_req* tempo_req_l_;
  Audio_tempo* audio_p_;
};

#endif // TEMPO_PERFORMER_HH
