#ifndef MOLECULE_HH
#define MOLECULE_HH

#include "proto.hh"
#include "plist.hh"
#include "boxes.hh"
#include "symbol.hh"

/// a symbol which can be translated, and freely copied
struct Atom {
    Offset off;
    Symbol sym;

    /* *************** */
    
    void translate(Offset o) {
	off += o;
    }
    
    /// how big is #this#?
    Box extent() const;
    Atom(Symbol s);

    void print() const;

    String TeXstring() const;
};


/** a group of individually translated symbols. You can add molecules
    to the top, to the right, etc.  */
struct Molecule {
    Pointer_list<Atom*> ats;	// change to List<Atom>? 

    /* *************** */
    
    Molecule() { }
    Molecule(Atom a) { add(a) ; }

    void add_right(const Molecule &m);
    void add_left(const Molecule &m);
    void add_top(const Molecule &m);
    void add_bottom(const Molecule &m);
    void add(Molecule const &m);
    void translate(Offset);
    void add(Atom a) { ats.bottom().add(new Atom(a)); }
    /// how big is #this#? 
    Box extent() const;

    String TeXstring() const;

    Molecule(const Molecule&s);
    void print() const;
private:
    void operator=(const Molecule&);
};
#endif
