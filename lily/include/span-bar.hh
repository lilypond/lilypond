/*
  span-bar.hh -- declare Span_bar

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef SPAN_BAR_HH
#define SPAN_BAR_HH

#include "bar.hh"

class Span_bar : public virtual Bar
{
    Link_array<Score_elem> spanning_l_arr_;

public:
    Span_bar();
    DECLARE_MY_RUNTIME_TYPEINFO;
    SCORE_ELEM_CLONE(Span_bar);
    void add (Bar*);
    void set (Vertical_align_element *);

protected:
    virtual Interval do_width() const;
    virtual void do_pre_processing();
    
    virtual void do_substitute_dependency (Score_elem*,Score_elem*);
    virtual Molecule * brew_molecule_p() const;
    virtual Atom get_bar_sym (Real dy) const;
};

#endif // SPAN_BAR_HH
