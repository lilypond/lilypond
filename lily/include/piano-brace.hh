/*   
  piano-brace.hh -- declare Piano_brace
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef PIANO_BRACE_HH
#define PIANO_BRACE_HH

#include "span-score-bar.hh"

class Piano_brace : public Span_score_bar
{
public:
  
  VIRTUAL_COPY_CONS(Score_element);

  /** make room for Staff_bracket.  Ugh.  Should use some kind of
    relation thingy.  */
  Real extra_move_left_f_;
  Piano_brace ();
protected:
  virtual Interval do_width() const;
  virtual void do_post_processing();
  virtual Atom get_bar_sym (Real) const;
};


#endif /* PIANO_BRACE_HH */

