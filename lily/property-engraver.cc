/*   
  property-engraver.cc --  implement Property engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "lily-guile.hh"
#include "engraver.hh"
#include "protected-scm.hh"
#include "dictionary.hh"
#include "score-element.hh"

class Property_engraver : public Engraver
{
  Dictionary<Protected_scm> prop_dict_;
  void apply_properties (SCM, Score_element*);

protected:
  virtual void acknowledge_element (Score_element_info ei);
  virtual void do_creation_processing ();

  VIRTUAL_COPY_CONS(Translator);
};

void
Property_engraver::do_creation_processing ()
{
  SCM plist = get_property ("Generic_property_list", 0);
  for (; SCM_NIMP (plist); plist = gh_cdr (plist))
    {
      SCM elt_props = gh_car (plist);
      prop_dict_[ly_scm2string (gh_car (elt_props))] = gh_cdr (elt_props);
    }
}

void
Property_engraver::acknowledge_element (Score_element_info i)
{
  if (prop_dict_.elem_b (i.elem_l_->name()))
    {
      SCM p = prop_dict_[i.elem_l_->name()];
      apply_properties (p,i.elem_l_);
    }
  if (prop_dict_.elem_b ("all"))
    {
      apply_properties (prop_dict_["all"], i.elem_l_);
    }
}

void
Property_engraver::apply_properties (SCM p, Score_element *e)
{  
  for (; gh_pair_p (p); p = gh_cdr (p))
    {
      SCM entry = gh_car (p);
      SCM prop_sym = gh_car (entry);
      SCM type_p   = gh_cadr (entry);
      SCM elt_prop_name = gh_caddr (entry);

      SCM preset = scm_assq(prop_sym, e->element_property_alist_);
      if (preset != SCM_BOOL_F)
	continue;
  
      SCM val = get_property (prop_sym, 0);
      if (val != SCM_UNDEFINED
	  && gh_apply (type_p, scm_listify (val, SCM_UNDEFINED))
	  == SCM_BOOL_T)
	e->set_elt_property (ly_symbol2string (elt_prop_name), val);
    }
}

ADD_THIS_TRANSLATOR(Property_engraver);
