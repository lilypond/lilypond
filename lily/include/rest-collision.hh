/*
  rest-collision.hh -- declare Rest_collision

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef REST_COLLISION_HH
#define REST_COLLISION_HH

#include "lily-proto.hh"
#include "lily-guile.hh"



/*
  Move rests in note-columns so that they do not collide.
  
  properties:

  read-only

  maximum-rest-count -- kill off rests so we don't more than this
    number left.

  minimum-distance -- minimum distance between notes and rests.

  read/write
  
  elements -- list of elts (both rests and notes) participating in the
    collision.


  sets in elements:

    rest-collision -- pointer to self.

    
  
    
*/

class Rest_collision		// interface
{
public:
  static void add_column (Score_element*me,Score_element*);
  static void set_interface (Score_element*me);
  static bool has_interface (Score_element*);
  static Real force_shift_callback (Score_element *, Axis);
  static SCM do_shift (Score_element*,SCM);
};
#endif // REST_COLLISION_HH
