/*   
  separating-line-group-engraver.hh -- declare Separating_line_group_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef SEPARATING_LINE_GROUP_GRAV_HH
#define SEPARATING_LINE_GROUP_GRAV_HH

#include "engraver.hh"

class Separating_line_group_engraver : public Engraver
{
protected:
  Single_malt_grouping_item * break_malt_p_;
  Single_malt_grouping_item* nobreak_malt_p_;
  Separating_group_spanner * sep_span_p_;
  
  virtual void acknowledge_element (Score_element_info);
  virtual void do_creation_processing ();
  virtual void do_removal_processing ();
  virtual void do_pre_move_processing ();
public:
  Separating_line_group_engraver ();
  VIRTUAL_COPY_CONS (Translator);
  
};


#endif /* SEPARATING_LINE_GROUP_GRAV_HH */

