/*
  rest.hh -- part of LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef REST_HH
#define REST_HH
#include "item.hh"

/** typeset a Rest. A "vanilla" item.
 */
struct Rest : Item {

    int dots;
    int balltype;
    
    /// rests can be translated up and down.
    int pos_i_;
    /* *************** */


    Rest(Duration);
    void do_print()const;
NAME_MEMBERS(Rest);
    Molecule* brew_molecule_p()const;
};
#endif 

