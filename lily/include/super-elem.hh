/*
  super-elem.hh -- declare Super_elem

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef SUPER_ELEM_HH
#define SUPER_ELEM_HH

#include "score-elem.hh"
/** The toplevel element. The Paper_score contains this element, and any
  element shoud be a dependency for the super element.
  */
class Super_elem : public Score_elem {
public:
    Link_array<Line_of_score> lines_arr_;
    Line_of_score * line_of_score_l_;
    void add_broken_line (Line_of_score*);
    Super_elem();
    virtual String TeX_output_str() const;
protected:
    virtual void do_substitute_dependency (Score_elem*,Score_elem*);
    virtual void handle_broken_dependencies();

    virtual void do_add_processing();
    DECLARE_MY_RUNTIME_TYPEINFO;
};

#endif // SUPER_ELEM_HH
