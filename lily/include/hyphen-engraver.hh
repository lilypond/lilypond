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
#include "extender-engraver.hh"


/**
  Generate an centred hyphen.  Should make a Hyphen_spanner that typesets
  a nice centred hyphen of varying length depending on the gap between syllables.

  We remember all Text_items that come across, and store their
  termination times. When we get a request, we create the spanner, and
  attach the left point to the finished lyrics, and the right point to
  any lyrics we receive by then.
*/
class Hyphen_engraver : public Engraver
{
  PQueue<Text_lyric_tuple> past_lyrics_pq_;
  Array<Text_lyric_tuple> now_lyrics_;
  Array<Text_lyric_tuple> stopped_lyrics_;  
  
  Hyphen_req* req_l_;
  Hyphen_spanner* hyphen_spanner_p_;

  
public:
  Hyphen_engraver ();
  VIRTUAL_COPY_CONS (Translator);

protected:
  virtual void acknowledge_element (Score_element_info);
  virtual void do_removal_processing();
  virtual void do_process_requests();
  virtual bool do_try_music (Music*);
  virtual void do_pre_move_processing();
  virtual void do_post_move_processing ();
private:

};

#endif // HYPHEN_ENGRAVER_HH
