/*
axis-group.hh -- declare Axis_group_administration, Axis_group_element

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef Axis_group_administration_HH
#define Axis_group_administration_HH

#include "parray.hh"
#include "axes.hh"
#include "real.hh"
#include "lily-proto.hh"
#include "score-elem.hh"
/**
  Do the dirty work for Axis_group_element.
 */
struct Axis_group_administration {
    Link_array<Score_elem> elem_l_arr_;
    
    Interval extent(Axis)const;
    void print() const ;
    Axis_group_administration(Axis_group_administration const&);
    Axis_group_administration(){}
    void remove_all(Axis a1,Axis a2);
    
    bool contains_b(Score_elem const *)const;
    void add_element(Score_elem*, Axis_group_element*, Axis a1, Axis a2);
    void remove_element(Score_elem*, Axis a1, Axis a2);
};

/** 
  Treat a group of elements a unity in either or both axis sense .
  This is a wrapper around Axis_group_administration
  */
class Axis_group_element : public virtual Score_elem {
protected:
    Axis_group_administration axis_admin_;
    virtual void do_print()const;
    virtual Link_array<Score_elem> get_extra_dependencies()const;
    virtual void do_unlink();

public:
    virtual void remove_all()=0;
    virtual void add_element(Score_elem*)=0;
    virtual void remove_element(Score_elem*)=0;
    virtual bool contains_b(Score_elem const *)const;
    DECLARE_MY_RUNTIME_TYPEINFO;
};
#endif // Axis_group_administration_HH
