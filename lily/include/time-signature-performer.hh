/*
  time_signature-performer.hh -- declare Time_signature_performer

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef TIME_SIGNATURE_PERFORMER_HH
#define TIME_SIGNATURE_PERFORMER_HH

#include "lily-proto.hh"
#include "performer.hh"

class Time_signature_performer : public Performer
{
public:
  VIRTUAL_COPY_CONS(Translator);
  
  Time_signature_performer();
  ~Time_signature_performer();

protected:
  void do_print() const;
  virtual bool do_try_music (Music* req_l);
  virtual void do_process_requests();
  virtual void do_pre_move_processing ();

private:
  Time_signature_change_req* time_signature_req_l_;
  Audio_time_signature* audio_p_;
};

#endif // TIME_SIGNATURE_PERFORMER_HH
