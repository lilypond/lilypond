/*   
  axis-group-engraver.hh -- declare Axis_group_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef AXIS_GROUP_ENGRAVER_HH
#define AXIS_GROUP_ENGRAVER_HH


#include "engraver.hh"

/**
   Put stuff in a Axis_group_spanner.  Use as last element of a context. 
 */
class Axis_group_engraver : public Engraver
{
protected:
  Spanner *staffline_p_;
  Link_array<Score_element> elts_;

  virtual void do_creation_processing();
  virtual void do_removal_processing();
  virtual void acknowledge_element (Score_element_info);
  virtual void process_acknowledged ();
  virtual Spanner* get_spanner_p () const;
public:
  VIRTUAL_COPY_CONS(Translator);
  
  Axis_group_engraver ();
};

#endif /* AXIS_GROUP_ENGRAVER_HH */

