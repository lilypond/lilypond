/*   
  group-interface.hh -- declare Group_interface
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef GROUP_INTERFACE_HH
#define GROUP_INTERFACE_HH

#include "lily-proto.hh"
#include "string.hh"
#include "lily-guile.hh"
#include "smobs.hh"

/**
   Look at Score element ELT as thing which has a list property called
   NAME_. Normally the list would contain Score_elements, but
   sometimes it can be different things.

   todo: reename as list_interface?
*/

struct Group_interface
{
  Score_element * elt_l_;
  String name_;
public:
  Group_interface (Score_element const*);
  Group_interface (Score_element const*, String);
  int count ();
  void add_thing (SCM);
  bool has_interface_b ();
  void set_interface ();
};

struct Pointer_group_interface {
  Score_element * elt_l_;
  String name_;
public:
  Pointer_group_interface (Score_element const*);
  Pointer_group_interface (Score_element const*, String);
  int count ();
  void set_interface ();
  bool has_interface_b ();
  void add_element (Score_element*);
};
/** 
  Put all score elements of ELT's property called NAME into an array,
  and return it.  */
template<class T>
Link_array<T>
Pointer_group_interface__extract_elements (Score_element const *elt, T *, const char* name)
{
  Link_array<T> arr;

  for (SCM s = elt->get_elt_pointer (name); gh_pair_p (s); s = gh_cdr (s))
    {
      SCM e = gh_car (s);
      arr.push (dynamic_cast<T*> (unsmob_element (e)));
    }

  arr.reverse ();
  return arr;
}




#endif /* GROUP_INTERFACE_HH */

