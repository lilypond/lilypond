/*   
  System-start-delimiter.hh -- declare System_start_delimiter
     
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef SYSTEM_START_DELIMITER_HH
#define SYSTEM_START_DELIMITER_HH

#include "spanner.hh"

/*
  Braces/brackets across staffs.
 */
class System_start_delimiter : public Spanner
{
public:
  System_start_delimiter (SCM);
  static SCM scheme_molecule (SCM);
  
  VIRTUAL_COPY_CONS (Score_element);
protected:
  virtual void after_line_breaking();
  
  Molecule staff_bracket (Real) const;
  Molecule staff_brace (Real) const;
  Molecule simple_bar (Real) const;
};

#endif /* SYSTEM_START_DELIMITER_HH */

