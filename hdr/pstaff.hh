#ifndef PSTAFF_HH
#define PSTAFF_HH

#include "plist.hh"
#include "item.hh"
#include "symbol.hh"

/// items grouped vertically.
struct PStaff {
    Parametric_symbol *stafsym;
    PScore * pscore_;
    
    virtual Symbol get_stafsym(Real width)const=0; // maybe overkill

    List<const Spanner*> spans;
    List<Item*> its;

    void add(Item*i);
    PStaff(PScore*);
    virtual ~PStaff() {}
};

#endif
