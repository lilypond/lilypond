/*
  extender-engraver.hh -- declare Extender_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1998--1999 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef EXTENDER_ENGRAVER_HH
#define EXTENDER_ENGRAVER_HH

#include "engraver.hh"
#include "drul-array.hh"
#include "extender-spanner.hh"
#include "pqueue.hh"

struct Text_lyric_tuple {
  Rhythmic_req *req_l_ ;
  Text_item *text_l_;
  Moment end_;
  
  Text_lyric_tuple ();
  Text_lyric_tuple (Text_item*, Rhythmic_req*, Moment);
  static int time_compare (Text_lyric_tuple const &, Text_lyric_tuple const &);
};

inline int compare (Text_lyric_tuple const &a, Text_lyric_tuple const &b)
{
  return Text_lyric_tuple::time_compare (a,b);
}




/**
  Generate an extender.  Should make an Extender_spanner that typesets
  a nice extender line.

  We remember all Text_items that come across, and store their
  termination times. When we get a request, we create the spanner, and
  attach the left point to the finished lyrics, and the right point to
  any lyrics we receive by then.
*/
class Extender_engraver : public Engraver
{
  PQueue<Text_lyric_tuple> past_lyrics_pq_;
  Array<Text_lyric_tuple> now_lyrics_;
  Array<Text_lyric_tuple> stopped_lyrics_;  
  
  Extender_req* req_l_;
  Extender_spanner* extender_spanner_p_;

  
public:
  Extender_engraver ();
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

#endif // EXTENDER_ENGRAVER_HH
