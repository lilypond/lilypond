/*
  notehead.hh -- part of LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef NOTEHEAD_HH
#define NOTEHEAD_HH

#include "item.hh"

/** ball at the end of the stem takes care of:

  * help lines  
  * proper placing of dots 

  It also is the item for a Rest
  
  */

class Notehead : public Item {
public:
    NAME_MEMBERS(Notehead);

    bool rest_b_;
    int position_i_;
    
    /// -1 = lowest, 0 = inside, 1 = top
    int extremal_i_;
    
    /// needed for the help-lines
    int staff_size_i_;
    int dots_i_;
    int balltype_i_;
    int x_dir_i_;
    
    /* *************** */
    
    void set_rhythmic(Rhythmic_req *);

    /**
      position of top line (5 linestaff: 8)
      */
    Notehead(int staff_size);
    static int compare(Notehead * const &a, Notehead *const &b) ;
protected:
    virtual    void do_print()const;
    virtual    Molecule* brew_molecule_p()const;
};
#endif // NOTEHEAD_HH

