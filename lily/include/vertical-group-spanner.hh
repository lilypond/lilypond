/*
  vertical-group-spanner.hh -- declare Vertical_group_spanner

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef SPAN_VERTICAL_GROUP_HH
#define SPAN_VERTICAL_GROUP_HH

#include "spanner.hh"
#include "elem-group.hh"

/** An element which groups a line. Due to technical problems, this
   cannot be used as a baseclass */
class Vertical_group_spanner : public Spanner, public Vertical_group
{
protected:
    virtual void do_break_processing();
    virtual void do_print()const;
    VIRTUAL_COPY_CONS(Vertical_group_spanner, Score_elem);

private:
    void remove_all();
    /// shouldn't be copied.
    Vertical_group_spanner(Vertical_group_spanner const&);

public:
    Vertical_group_spanner();
    DECLARE_MY_RUNTIME_TYPEINFO;
};

#endif // SPAN_VERTICAL_GROUP_HH
