#ifndef MOLECULE_HH
#define MOLECULE_HH

#include "list.hh"
#include "boxes.hh"
#include "item.hh"

/// a symbol which can be translated, and freely copied
struct Atom {
    Offset off;
    const Symbol * sym;

    void translate(Offset o) {
	off += o;
    }
    
    /// how big is #this#?
    Box extent() const {
	Box b( sym->dim);
	b.translate(off);
	return b;
    }
    Atom(const Symbol*s) {
	sym=s;
    }
    String TeXstring() const;
};

/// a group of #Atom#s
struct Molecule : Output {
    List<Atom> ats;

    Molecule() { }
    Molecule(Atom a) { ats.bottom().add(a); }
    //    Molecule(Molecule const&src);
    void add_right(const Molecule &m);
    void add_left(const Molecule &m);
    void add_top(const Molecule &m);
    void add_bot(const Molecule &m);
    void add(Molecule const &m);
    void translate(Offset);

    /// how big is #this#? 
    Box extent() const;

    String TeXstring() const;
};
/** a group of individually translated symbols. You can add molecules
    to the top, to the right, etc.  */
#endif
