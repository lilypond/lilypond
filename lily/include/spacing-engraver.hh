/*   
  spacing-engraver.hh -- declare Spacing_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef SPACING_ENGRAVER_HH
#define SPACING_ENGRAVER_HH

#include "engraver.hh"
#include "pqueue.hh"

struct Rhythmic_tuple
{
  Score_element_info info_;
  Moment end_;
  
  Rhythmic_tuple ()
    {
    }
  Rhythmic_tuple (Score_element_info i, Moment m )
    {
      info_ = i;
      end_ = m;
    }
  static int time_compare (Rhythmic_tuple const &, Rhythmic_tuple const &);  
};

/**
   Acknowledge rhythmic elements, for initializing spacing fields in
   the columns.

   should be the  last one of the toplevel context
*/
class Spacing_engraver : public Engraver
{
  PQueue<Rhythmic_tuple> playing_durations_;
  Array<Rhythmic_tuple> now_durations_;
  Array<Rhythmic_tuple> stopped_durations_;

  Spacing_spanner * spacing_p_;
protected:
  VIRTUAL_COPY_CONS(Translator);
  virtual void acknowledge_element (Score_element_info);
  virtual void do_post_move_processing ();
  virtual void do_pre_move_processing ();
  virtual void do_creation_processing ();
  virtual void do_removal_processing ();
public:
  Spacing_engraver ();
};

#endif /* SPACING_ENGRAVER_HH */

