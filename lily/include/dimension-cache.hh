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
typedef Interval (*Dim_cache_callback)(Dimension_cache const *);
typedef Real (*Offset_cache_callback)(Dimension_cache const *);

/**
  Adminstration of offset dimension info. 
 */
class Dimension_cache
{
  bool valid_b_;
  Interval dim_;
  /**
    The offset wrt. to the center of #parent_l_#
   */

  Real extra_offset_;
  Real basic_offset_;
  bool off_valid_b_;

  Score_element *elt_l_;
  friend class Score_element;

  void init ();
public:
  Array<Offset_cache_callback> off_callbacks_;
  /**
     What to call to find extent.  Nil means empty. 
   */
  Dim_cache_callback callback_l_;
  static Interval point_dimension_callback (Dimension_cache const* );
  Axis axis () const;
  Real get_offset () const;
  void set_callback (Dim_cache_callback);
  /** The #offset_# is defined with regard to this graphical_element/
    dimension_cache.  */
  void set_offset_callback (Offset_cache_callback);
  Dimension_cache * parent_l_;

  Score_element *element_l () const { return elt_l_; }

  Dimension_cache(Dimension_cache const&);
  Dimension_cache ();


  /**
     Find the offset relative to D.  If   D equals THIS, then it is 0.
     Otherwise, it recursively defd as

     OFFSET_ + PARENT_L_->relative_coordinate (D)
   */
  Real relative_coordinate (Dimension_cache *d) const;
  Dimension_cache*common_refpoint (Dimension_cache const* s) const;
  Dimension_cache*common_refpoint (Link_array<Dimension_cache> caches) const;
  void set_empty (bool);
  void translate (Real);

  bool valid_b () const { return valid_b_; }
  bool empty_b() const;
  Interval get_dim () const;
};


#endif /* DIMENSION_CACHE_HH */

