#ifndef PSTAFF_HH
#define PSTAFF_HH

#include "list.hh"
#include "item.hh"
#include "symbol.hh"

/// items grouped vertically.
class PStaff {

public:
    Parametric_symbol *stafsym;
    virtual Symbol get_stafsym(Real width)const=0; // mayybe overkill

    List<const Spanner*> spans;
    List<Item*> its;

    void add(Item*i);
    PStaff();
    virtual ~PStaff() {}
};

#endif
