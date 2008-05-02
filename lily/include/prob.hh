/*
  property-object.hh -- declare Property_object

  source file of the GNU LilyPond music typesetter

  (c) 2004--2007  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef PROPERTY_OBJECT_HH
#define PROPERTY_OBJECT_HH

#include "stencil.hh"
#include "virtual-methods.hh"


/*
  A formatted "system" (A block of titling also is a Property_object)

  To save memory, we don't keep around the System grobs, but put the
  formatted content of the grob is put into a
  Property_object. Page-breaking handles Property_object objects.
*/

class Prob
{
  DECLARE_SMOBS (Prob);
  DECLARE_CLASSNAME(Prob);

  void init_vars ();
protected:
  SCM mutable_property_alist_;
  SCM immutable_property_alist_;
  SCM type_;
  
  virtual void derived_mark () const;
  virtual SCM copy_mutable_properties () const;
  virtual void type_check_assignment (SCM,SCM) const;
  
public:
  Prob (SCM, SCM);
  Prob (Prob const &);
  virtual string name () const;
  SCM type () const { return type_; }
  SCM get_property_alist (bool _mutable) const;
  SCM internal_get_property (SCM sym) const;
  void instrumented_set_property (SCM, SCM, const char*, int, const char*);
  void internal_set_property (SCM sym, SCM val);
};

DECLARE_UNSMOB(Prob,prob);

SCM ly_prob_set_property_x (SCM system, SCM sym, SCM value);
SCM ly_prob_property (SCM system, SCM sym, SCM dfault);

SCM ly_prob_type_p (SCM obj, SCM sym);
  
#endif /* PROPERTY_OBJECT_HH */
