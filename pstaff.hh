#ifndef PSTAFF_HH
#define PSTAFF_HH

#include "list.hh"
#include "item.hh"

/// items grouped vertically.
class PStaff {
public:
    Stretchable_symbol *stafsym;
    List<const Spanner*> spans;
    List<Item*> its;

    void add(Item*i);
    PStaff();
    virtual ~PStaff() {}
};

#endif
