/*
  repeat-engraver.hh -- declare Repeat_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1998 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef REPEAT_ENGRAVER_HH
#define REPEAT_ENGRAVER_HH

#include "engraver.hh"

/**
  Generate repeat-bars |: :| for repeated-music
  */
class Repeat_engraver : public Engraver 
{
public:
  Repeat_engraver ();
  VIRTUAL_COPY_CONS(Translator);
  
protected:
  virtual void acknowledge_element (Score_element_info i);
  virtual void do_removal_processing ();
  virtual bool do_try_music (Music *req_l);
  virtual void do_process_requests();
  virtual void do_pre_move_processing();
  virtual void do_post_move_processing();

private:
  Link_array<Repeated_music> repeated_music_arr_;
  Link_array<Music> alternative_music_arr_;
  //  Link_array<Bar> bar_p_arr_;
  Array<bool> bar_b_arr_;
  Link_array<Volta_spanner> volta_p_arr_;
  Array<Moment> stop_mom_arr_;
  Array<Moment> alternative_start_mom_arr_;
  Array<Moment> alternative_stop_mom_arr_;
  Array<String> alternative_str_arr_;
};

#endif // REPEAT_ENGRAVER_HH

