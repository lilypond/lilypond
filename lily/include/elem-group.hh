/*
  elem-group.hh -- declare Horizontal_vertical_group

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef ELEM_GROUP_HH
#define ELEM_GROUP_HH
#include "score-elem.hh"

class Elbement_group: public virtual Score_elem {
protected:
    Link_array<Score_elem> elem_l_arr_;
    
    virtual void do_print() const ;
public:
    Elbement_group();
    bool contains_b(Score_elem const *)const;
    void add_element(Score_elem*);
    NAME_MEMBERS();
       
};

class Horizontal_group : public Elbement_group {
public:
    NAME_MEMBERS();
    Horizontal_group(Horizontal_group const&);
    Horizontal_group(){}
    void add_element(Score_elem*);
protected:
    virtual void do_substitute_dependency(Score_elem*,Score_elem*);
    virtual void do_print() const ;
  virtual void translate_x(Real);
    virtual Interval do_width()const;
};

class Vertical_group : public Elbement_group {
protected:
    virtual void translate_y(Real);
    virtual void do_print() const ;
  virtual Interval do_height()const;
    virtual void do_substitute_dependency(Score_elem*,Score_elem*);
    void add_element(Score_elem*);
public:
    Vertical_group(Vertical_group const &);
    Vertical_group(){}
    NAME_MEMBERS();
};

/** A class to treat a group of elements as a single entity. The
  dimensions are the unions of the dimensions of what it contains.
  Translation means translating the contents.
  */
class Horizontal_vertical_group : public Vertical_group, 
				  public Horizontal_group 
{  
protected:
    virtual Horizontal_vertical_group* elem_group() { return this; }
public:
    Element_group();
    bool contains_b(Score_elem const *)const;
    NAME_MEMBERS();
    virtual void do_substitute_dependency(Score_elem*,Score_elem*);
    virtual void do_print() const;
    void add_element(Score_elem*);
};

#endif // ELEM_GROUP_HH
