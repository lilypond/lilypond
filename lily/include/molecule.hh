#ifndef MOLECULE_HH
#define MOLECULE_HH

#include "lily-proto.hh"
#include "plist.hh"
#include "boxes.hh"


/** a group of individually translated symbols. You can add molecules
    to the top, to the right, etc.  */
struct Molecule {
    Pointer_list<Atom*> ats;	// change to List<Atom>? 

    /* *************** */
    
    Molecule() { }
    Molecule(Atom const &a) { add(a) ;}

    void add_right(const Molecule &m);
    void add_left(const Molecule &m);
    void add_top(const Molecule &m);
    void add_bottom(const Molecule &m);
    void add(Molecule const &m);
    void translate(Offset);
    void translate(Real,Axis);
    void add(Atom const & a) ;
    /// how big is #this#? 
    Box extent() const;

    String TeX_string() const;

    Molecule(const Molecule&s);
    void print() const;
private:
    void operator=(const Molecule&);
};
#endif
