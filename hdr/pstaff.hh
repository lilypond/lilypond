#ifndef PSTAFF_HH
#define PSTAFF_HH

#include "plist.hh"
#include "item.hh"
#include "symbol.hh"

/// items grouped vertically.
struct PStaff {
    Parametric_symbol *stafsym;
    PScore * pscore_;
    
    
    PointerList<const Spanner*> spans;
    PointerList<Item*> its;

    /****************/
    virtual Symbol get_stafsym(Real width)const=0; // maybe overkill
    void add(Item*i);
    PStaff(PScore*);
    virtual ~PStaff() {}
};

#endif
