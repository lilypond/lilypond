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
    Elbement_group(Elbement_group const&);
    Elbement_group();
    bool contains_b(Score_elem const *)const;
    void add_element(Score_elem*);
    DECLARE_MY_RUNTIME_TYPEINFO;
};

class Horizontal_group : public Elbement_group {
public:
    DECLARE_MY_RUNTIME_TYPEINFO;
    Horizontal_group(){}
    void add_element(Score_elem*);
    Horizontal_group(Horizontal_group const&);
    void remove_element(Score_elem*);

protected:
    virtual Horizontal_group * horizontal_group() { return this; }
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
    virtual Vertical_group * vertical_group() { return this; }
public:
    void add_element(Score_elem*);
    void remove_element(Score_elem*);
    
    Vertical_group(Vertical_group const &);
    Vertical_group(){}
    DECLARE_MY_RUNTIME_TYPEINFO;
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
    Horizontal_vertical_group();
    bool contains_b(Score_elem const *)const;
    DECLARE_MY_RUNTIME_TYPEINFO;
    virtual void do_substitute_dependency(Score_elem*,Score_elem*);
    virtual void do_print() const;
    void add_element(Score_elem*);
};

#endif // ELEM_GROUP_HH
