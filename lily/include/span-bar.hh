/*
  span-bar.hh -- declare Span_bar

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef SPAN_BAR_HH
#define SPAN_BAR_HH

#include "bar.hh"

class Span_bar : public Bar
{
    Link_array<Bar> spanning_l_arr_;

public:
    NAME_MEMBERS();
    SCORE_ELEM_CLONE(Span_bar);
    void add(Bar* );
    void set( Vertical_align_element *);
    void do_pre_processing();
    void do_substitute_dependency(Score_elem*,Score_elem*);
    Molecule * brew_molecule_p()const;
};
#endif // SPAN_BAR_HH
