/*
  span-bar.hh -- declare Span_bar

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef SPAN_BAR_HH
#define SPAN_BAR_HH

#include "bar.hh"

class Span_bar : public virtual Bar
{
    Link_array<Score_element> spanning_l_arr_;

public:
    Span_bar();
    DECLARE_MY_RUNTIME_TYPEINFO;
    SCORE_ELEMENT_CLONE(Span_bar);
    void add_bar (Bar*);
    void set_align (Align_element *);

protected:
    void evaluate_empty ();
    virtual Interval do_width() const;
    virtual void do_pre_processing();
    virtual void do_post_processing();
    
    virtual void do_substitute_dependency (Score_element*,Score_element*);
    virtual Molecule * brew_molecule_p() const;
    virtual Atom get_bar_sym (Real dy) const;
};

#endif // SPAN_BAR_HH
