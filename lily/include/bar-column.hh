/*
  bar-column.hh -- declare Bar_column

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef BAR_COLUMN_HH
#define BAR_COLUMN_HH

#include "script-column.hh"

/**  
  The scripts  over  a and the bar itself.
 */
class Bar_column : public Script_column {
    Bar * bar_l_;

public:
    Bar_column();
    void set_bar (Bar*);
protected:
    SCORE_ELEMENT_CLONE(Bar_column);
    void do_substitute_dependency (Score_element*,Score_element*);
    DECLARE_MY_RUNTIME_TYPEINFO;
};

#endif // BAR_COLUMN_HH
