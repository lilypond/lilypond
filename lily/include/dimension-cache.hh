/*   
  dimension-cache.hh -- declare Dimension_cache
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef DIMENSION_CACHE_HH
#define DIMENSION_CACHE_HH

#include "interval.hh"
#include "real.hh"
#include "lily-proto.hh"
#include "parray.hh"

class Dimension_cache;
typedef Interval (*Dim_cache_callback)(Dimension_cache *);

/**
  Adminstration of offset  dimension info. 
 */
class Dimension_cache
{
  bool valid_b_;
  /** Is this element dimensionless?.
    This is private to guarantee correctness of the cache
   */
  bool empty_b_;
  Interval dim_;
  /**
    The offset wrt. to the center of #parent_l_#
   */
  Real offset_;
  Graphical_element *elt_l_;
  Dim_cache_callback callback_l_;
  friend class Graphical_element;

  void init ();
public:
  Real offset () const;
  void set_callback (Dim_cache_callback);
  /** The #offset_# is defined with regard to this graphical_element/
    dimension_cache.  */
  
  Dimension_cache * parent_l_;
  Link_array<Dimension_cache> dependencies_l_arr_;
  Graphical_element *element_l () { return elt_l_; }
  Real absolute_coordinate () const;
  void invalidate ();
  void invalidate_dependencies ();
  
  Dimension_cache(Dimension_cache const&);
  Dimension_cache ();

  Real relative_coordinate (Dimension_cache *d) const;
  Dimension_cache*common_group (Dimension_cache const* s) const;
  Dimension_cache*common_group (Link_array<Dimension_cache> caches) const;
  void set_empty (bool);
  void translate (Real);
  void set_offset (Real);
  bool valid_b () const { return valid_b_; }
  bool empty_b() const { return empty_b_; }
  void set_dim (Interval);
  Interval get_dim () const;
};


#endif /* DIMENSION_CACHE_HH */

