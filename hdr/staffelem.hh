/*
  staffelem.hh -- part of LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef STAFFELEM_HH
#define STAFFELEM_HH
#include "varray.hh"
#include "proto.hh"
#include "offset.hh"
#include "molecule.hh"

struct Staff_elem {
    enum Status {
	ORPHAN,			// not yet added to pstaff
	VIRGIN,			// added to pstaff
	PRECALCED,		// calcs before spacing done
	POSTCALCED,		// after spacing calcs done
	OUTPUT,			// molecule has been output
    } status;
    bool calc_children;
    Array<Staff_elem*> dependencies;
    
    ///  the pstaff it is in
    PStaff *pstaff_l_;

    /****************/
    Staff_elem(Staff_elem const&);
    String TeXstring () const ;
    virtual void print() const;
    virtual Interval width() const;
    virtual Interval height() const;
    Paperdef *paper() const;
    virtual ~Staff_elem();
    Staff_elem();
    
    void translate(Offset);
    void add_processing();
    void pre_processing();
    void post_processing();
    void molecule_processing();
    virtual const char *name() const;		// to find out derived classes.
    virtual Spanner* spanner()  { return 0; }
    virtual Item * item() { return 0; }
protected:
    
    /// do printing of derived info.
    virtual void do_print() const=0;
    /// generate the molecule    
    virtual Molecule* brew_molecule_p()const=0;
    ///executed directly after the item is added to the PScore
    virtual void do_add_processing();
    /// do calculations before determining horizontal spacing
    virtual void do_pre_processing();

    /// do calculations after determining horizontal spacing
    virtual void do_post_processing();

private:
    /// member: the symbols
    Molecule *output;		// should scrap, and use temp var?

    /// 
    Offset offset_;
    /**
      This is  needed, because #output# may still be
      NULL.
      */
};
/** Both Spanner and Item are Staff_elem's. Most Staff_elem's depend
  on other Staff_elem's, eg, Beam needs to know and set direction of
  Stem. So the Beam has to be calculated *before* Stem. This is
  accomplished with the dependencies field of struct Staff_elem.

  */

#define NAME_METHOD(c)	const char *c::name()const{ return #c; } struct c
#endif // STAFFELEM_HH

