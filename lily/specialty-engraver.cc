/*   
  specialty-engraver.cc --  implement  Specialty_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */


#include "engraver.hh"

class Specialty_engraver : public Engraver
{

public:
  VIRTUAL_COPY_CONS(Translator);
protected:
  void acknowledge_element (Score_element_info);
};


void
Specialty_engraver::acknowledge_element (Score_element_info i)
{
  /*
    We could do groovy stuff, by inserting our own custom (FUNC,FONT)
    pairs (Atoms in fact) into acknowledged elements.

    But not yet.  This would be cleaner if we had SCM as properties.
  */
}
