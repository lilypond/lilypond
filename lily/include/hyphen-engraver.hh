/*
  hyphen-engraver.hh -- declare Hyphen_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1999 Glen Prideaux <glenprideaux@iname.com>
*/

#ifndef HYPHEN_ENGRAVER_HH
#define HYPHEN_ENGRAVER_HH

#include "engraver.hh"
#include "drul-array.hh"
#include "hyphen-spanner.hh"
#include "pqueue.hh"


/**
  Generate an centred hyphen.  Should make a Hyphen_spanner that
  typesets a nice centred hyphen of varying length depending on the
  gap between syllables.

  We remember the last Text_item that come across. When we get a
  request, we create the spanner, and attach the left point to the
  last lyrics, and the right point to any lyrics we receive by
  then.  */
class Hyphen_engraver : public Engraver
{
  Text_item *  last_lyric_l_;
  Text_item * current_lyric_l_;
  Hyphen_req* req_l_;
  Hyphen_spanner* hyphen_spanner_p_;
public:
  Hyphen_engraver ();
  VIRTUAL_COPY_CONS (Translator);

protected:
  virtual void acknowledge_element (Score_element_info);
  virtual void do_removal_processing();
  virtual void do_process_music();
  virtual bool do_try_music (Music*);
  virtual void do_pre_move_processing();
  virtual void do_post_move_processing ();
private:

};

#endif // HYPHEN_ENGRAVER_HH
