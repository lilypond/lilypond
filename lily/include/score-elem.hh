/*
  score-elem.hh -- part of GNU LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef STAFFELEM_HH
#define STAFFELEM_HH

#include "parray.hh"
#include "lily-proto.hh"
#include "offset.hh"
#include "molecule.hh"
#include "virtual-methods.hh"
#include "directed-graph.hh"


/** Both Spanner and Item are Score_elem's. Most Score_elem's depend
  on other Score_elem's, eg, Beam needs to know and set direction of
  Stem. So the Beam has to be calculated *before* Stem. This is
  accomplished with the dependencies fields of struct Score_elem,
  which are implemented in the Directed_graph_node class: all elements
  form an acyclic graph.

  (elem) */
class Score_elem : private Directed_graph_node {

    /// member: the symbols
    Molecule *output;		// should scrap, and use temp var?


    /**
      This is  needed, because #output# may still be
      NULL.
      */
    Offset offset_;

    enum Status {
	ORPHAN,			// not yet added to pstaff
	VIRGIN,			// added to pstaff
	PRECALCING,
	PRECALCED,		// calcs before spacing done
	BREAKING,
	BROKEN,
	POSTCALCING,		// busy calculating. This is used to trap cyclic deps.
	POSTCALCED,		// after spacing calcs done
	OUTPUT,			// molecule has been output
	DELMARKED,		// mark for 'unclean' deletion
	DELETED,		// to catch malloc mistakes.
    } status;


    Score_elem* dependency(int) const;
    Score_elem* dependent(int) const;
    int dependent_size() const;
    int dependency_size() const;
public:
    PScore *pscore_l_;    
    int group_element_i_;
        
    Score_elem(Score_elem const&);
    virtual String TeX_string () const ;
    virtual void print() const;
    
    Paper_def *paper() const;

    virtual ~Score_elem();
    Score_elem();
    NAME_MEMBERS(Score_elem);    
    virtual bool is_type_b(const char *);
    
    Interval width() const;
    Interval height() const;
     /**
      translate the symbol. The symbol does not have to be created yet. 
      Overridable, since this score-elem might act as a pseudo-list.
     */
    virtual void translate(Offset);
    Offset offset()const;

    void add_processing();
    void OK() const;
    void pre_processing();
    void break_processing();
    
    void post_processing();
    void molecule_processing();

    void unlink();
    void unlink_all();
    void remove_dependency(Score_elem*);
    /**
      add a dependency. It may be the 0 pointer, in which case, it is ignored.
     */
    void add_dependency(Score_elem* );    
    
    
    virtual Spanner* spanner()  { return 0; }
    virtual Element_group* elem_group() { return 0; }
    virtual Item * item() { return 0; }
    virtual Line_of_score * line_l() const;
protected:

    virtual  Interval do_height()const;
    virtual Interval do_width()const;
    
    /// do printing of derived info.
    virtual void do_print() const {}
    /// generate the molecule    
    virtual Molecule* brew_molecule_p()const;
    ///executed directly after the item is added to the PScore
    virtual void do_add_processing();
    /// do calculations before determining horizontal spacing
    virtual void do_pre_processing();

    
    /// do calculations after determining horizontal spacing
    virtual void do_post_processing();
    
    virtual void do_substitute_dependency(Score_elem * , Score_elem *);
    virtual void do_break_processing();
    virtual void handle_broken_dependencies();
};


#endif // STAFFELEM_HH

