/*   
  hara-kiri-engraver.hh -- declare Hara_kiri_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef HARA_KIRI_ENGRAVER_HH
#define HARA_KIRI_ENGRAVER_HH

#include "axis-group-engraver.hh"

class Hara_kiri_engraver : public Axis_group_engraver
{
protected:
  virtual Spanner*get_spanner_p ()const;
  virtual void acknowledge_element (Score_element_info);
public:
  VIRTUAL_COPY_CONS(Translator);
};

#endif /* HARA_KIRI_ENGRAVER_HH */
