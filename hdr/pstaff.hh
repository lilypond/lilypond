#ifndef PSTAFF_HH
#define PSTAFF_HH

#include "plist.hh"
#include "item.hh"
#include "symbol.hh"

/// items grouped horizontally
struct PStaff {
    Molecule * stafsym_p_;
    PScore * pscore_l_;
    
    
    PointerList<const Spanner*> spans;
    PointerList<Item*> its;

    /****************/
    virtual void brew_molecule(Real width)=0; // maybe overkill
    void add(Item*i);
    PStaff(PScore*);
    virtual ~PStaff();
private:
    PStaff(PStaff const&);
};

#endif
