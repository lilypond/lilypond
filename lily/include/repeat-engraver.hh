/*
  repeat-engraver.hh -- declare Repeat_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1998--2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef REPEAT_ENGRAVER_HH
#define REPEAT_ENGRAVER_HH

#include "engraver.hh"
#include "cons.hh"

struct Bar_create_event
{
  Moment when_;
  bool bar_b_;
  bool last_b_;
  String type_;
  Bar_create_event();
  Bar_create_event (Moment w, String s);
  Bar_create_event (Moment w, int i, int j);
};

int compare (Bar_create_event const & c1, Bar_create_event const &c2)
{
  return (c1.when_ - c2.when_).sign();
}

/**
  Generate repeat-bars |: :| for repeated-music
  */
class Repeat_engraver : public Engraver 
{
public:
  VIRTUAL_COPY_CONS(Translator);
  Repeat_engraver ();
protected:
  virtual void acknowledge_element (Score_element_info i);
  virtual void do_removal_processing ();
  virtual bool do_try_music (Music *req_l);
  virtual void do_process_music();
  virtual void do_pre_move_processing();
  virtual void do_post_move_processing ();
  void queue_events ();

private:
  Repeated_music *repeated_music_l_;
  bool done_this_one_b_;

  /*
    Royal_brackla_create_queue is only two Whiskies away. :-)
   */
  Cons<Bar_create_event> *create_barmoments_queue_;

  Volta_spanner * volta_span_p_;
  Volta_spanner* end_volta_span_p_;
};

#endif // REPEAT_ENGRAVER_HH

