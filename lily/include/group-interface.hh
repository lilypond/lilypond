/*   
  group-interface.hh -- declare Group_interface
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef GROUP_INTERFACE_HH
#define GROUP_INTERFACE_HH

#include "lily-proto.hh"
#include "string.hh"
#include "lily-guile.hh"
#include "smobs.hh"

struct Group_interface
{
  Score_element * elt_l_;
  String name_;
public:
  Group_interface (Score_element const*);
  Group_interface (Score_element const*, String);
  int count ();
  bool has_interface_b ();
  void set_interface ();
  void add_element (Score_element*);
};

/*
  template<class T>
  Link_array<T> Group_interface__extract_elements (T *, String name);
*/
template<class T>
Link_array<T>
Group_interface__extract_elements (Score_element const *elt, T *, String name)
{
  Link_array<T> arr;

  for (SCM s = elt->get_elt_property (name); gh_pair_p (s); s = gh_cdr (s))
    {
      SCM e = gh_car (s);
      assert (SMOB_IS_TYPE_B(Score_element,e));
      Score_element* se = SMOB_TO_TYPE(Score_element, e);
      arr.push (dynamic_cast<T*> (se));
    }

  arr.reverse ();
  return arr;
}




#endif /* GROUP_INTERFACE_HH */

