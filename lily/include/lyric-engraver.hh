/*
  lyric-engraver.hh -- declare Lyric_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef LYRIC_ENGRAVER_HH
#define LYRIC_ENGRAVER_HH

#include "lily-proto.hh"
#include "engraver.hh"
#include "array.hh"

class Lyric_engraver : public Engraver 
{
protected:
  virtual void do_pre_move_processing();
  virtual bool do_try_music (Music*);
  virtual void do_process_requests();

public:
  Lyric_engraver ();
  VIRTUAL_COPY_CONS (Translator);

private:
  Link_array<Lyric_req> lyric_req_l_arr_;
  Link_array<Item> text_p_arr_;
};


#endif // LYRIC_ENGRAVER_HH
