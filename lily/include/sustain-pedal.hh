/*   
  sustain-pedal.hh -- declare 
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef SUSTAIN_PEDAL_HH
#define SUSTAIN_PEDAL_HH

#include "item.hh" 


/*
  Urg.
  This is almost text
  Problem is:
    * we have no kerning
    * symbols are at wrong place in font



  Properties:

  glyph -- text string (TODO:   make one large glyph of the Ped symbol, removes need for do_brew_molecule ())

*/

class Sustain_pedal : public Item
{
public:
  VIRTUAL_COPY_CONS (Score_element);
  Sustain_pedal (SCM);
protected:
  virtual Molecule do_brew_molecule () const;
  virtual void after_line_breaking ();
};


#endif /* SUSTAIN_PEDAL_HH */

