/*
  span-brace-item.hh -- declare Span_brace_item

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
	   Jan Nieuwenhuizen <jan@digicash.com>
*/


#ifndef SPAN_BRACE_ITEM_HH
#define SPAN_BRACE_ITEM_HH

#include "bar.hh"

class Span_brace_item : public Item
{
public:
    NAME_MEMBERS();
    SCORE_ELEM_CLONE(Span_brace_item);

    void add( Bar* bar_l );
    void set( Vertical_align_element* vae_l );
    void do_pre_processing();
    void do_substitute_dependency( Score_elem*, Score_elem* );
    Molecule* brew_molecule_p() const;

private:
    Link_array<Bar> spanning_l_arr_;
};

#endif // SPAN_BRACE_ITEM_HH
