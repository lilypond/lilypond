/*
  extender-engraver.hh -- declare Extender_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1999 Glen Prideaux <glenprideaux@iname.com>,
  Han-Wen Nienhuys, Jan Nieuwenhuizen.
*/

#ifndef EXTENDER_ENGRAVER_HH
#define EXTENDER_ENGRAVER_HH

#include "engraver.hh"
#include "drul-array.hh"
#include "extender-spanner.hh"
#include "pqueue.hh"
#include "extender-engraver.hh"


/**
  Generate an centred extender.  Should make a Extender_spanner that
  typesets a nice centred extender of varying length depending on the
  gap between syllables.

  We remember the last Text_item that come across. When we get a
  request, we create the spanner, and attach the left point to the
  last lyrics, and the right point to any lyrics we receive by
  then.  */
class Extender_engraver : public Engraver
{
  Text_item *  last_lyric_l_;
  Text_item * current_lyric_l_;
  Extender_req* req_l_;
  Extender_spanner* extender_spanner_p_;
public:
  Extender_engraver ();
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

#endif // EXTENDER_ENGRAVER_HH
