/*
  lyric-engraver.hh -- declare Lyric_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef LYRIC_ENGRAVER_HH
#define LYRIC_ENGRAVER_HH

#include "lily-proto.hh"
#include "engraver.hh"
#include "array.hh"

/**
   Generate texts for lyric syllables.  We only do one lyric at a time.  
   Multiple copies of this engraver should be used to do multiple voices.
 */
class Lyric_engraver : public Engraver 
{
protected:
  virtual void do_pre_move_processing();
  virtual bool do_try_music (Music*);
  virtual void do_process_requests();
  virtual void do_post_move_processing ();
public:
  Lyric_engraver ();
  VIRTUAL_COPY_CONS (Translator);

private:
  Lyric_req * req_l_;
  Text_item* text_p_;
};


#endif // LYRIC_ENGRAVER_HH
