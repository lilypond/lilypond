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
  virtual void stop_translation_timestep();
  virtual bool try_music (Music*);
  void deprecated_process_music();
  virtual void start_translation_timestep ();
public:
  Lyric_engraver ();
  VIRTUAL_COPY_CONS (Translator);

private:
  Lyric_req * req_l_;
  Item* text_p_;
};


#endif // LYRIC_ENGRAVER_HH
