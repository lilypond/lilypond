
/*   
  staff-bracket.hh -- declare  Staff_bracket
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef STAFF_BRACKET_HH
#define STAFF_BRACKET_HH
#include "span-score-bar.hh"
class Staff_bracket : public Span_score_bar
{
public:
  
  VIRTUAL_COPY_CONS(Score_element);

protected:
  virtual Interval do_width() const;
  virtual void do_post_processing();
  virtual Molecule get_bar_sym (Real) const;
};


#endif /* STAFF_BRACKET_HH */

