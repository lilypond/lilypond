/*   
  property-engraver.cc --  implement Property engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "lily-guile.hh"
#include "engraver.hh"
#include "dictionary.hh"
#include "score-element.hh"
#include "scm-hash.hh"
#include "translator-group.hh"

/*
  JUNKME: should use pushproperty everywhere.
  
 */
class Property_engraver : public Engraver
{
  /*
    UGH. Junk Dictionary
  */
  Scheme_hash_table *prop_dict_;	// junkme
  void apply_properties (SCM, Score_element*, Translator_group *origin);

protected:
  virtual void acknowledge_element (Score_element_info ei);
  virtual void do_creation_processing ();
  virtual void do_removal_processing ();
public:
  ~Property_engraver();
  Property_engraver();
  VIRTUAL_COPY_CONS(Translator);
};



Property_engraver::Property_engraver()
{
  prop_dict_ = 0;
}
void
Property_engraver::do_removal_processing()
{
  
}

Property_engraver::~Property_engraver ()
{
  if (prop_dict_)
    scm_unprotect_object (prop_dict_->self_scm ());
}

void
Property_engraver::do_creation_processing ()
{
  prop_dict_ = new Scheme_hash_table;

  SCM plist = get_property (ly_symbol2scm ("Generic_property_list"));
  for (; gh_pair_p (plist); plist = gh_cdr (plist))
    {
      SCM elt_props = gh_car (plist);
      prop_dict_->set (gh_car (elt_props), gh_cdr (elt_props));
    }
}

void
Property_engraver::acknowledge_element (Score_element_info i)
{
  SCM ifs = i.elem_l_->get_elt_property ("interfaces");
  SCM props;
  for (; gh_pair_p (ifs); ifs = gh_cdr (ifs))
    {      
      if (prop_dict_->try_retrieve (gh_car (ifs), &props))
	{
	  apply_properties (props,i.elem_l_, i.origin_trans_l_->daddy_trans_l_);
	}
    }

  if (prop_dict_->try_retrieve (ly_symbol2scm ("all"), &props))
    {
      apply_properties (props, i.elem_l_, i.origin_trans_l_->daddy_trans_l_);
    }
}


void
Property_engraver::apply_properties (SCM p, Score_element *e, Translator_group*origin)
{
  for (; gh_pair_p (p); p = gh_cdr (p))
    {
      /*
	Try each property in order; earlier descriptions take
	precedence over later ones, and we don't touch elt-properties if
	they're already set.
      */
      
      SCM entry = gh_car (p);
      SCM prop_sym = gh_car (entry);
      SCM type_p   = gh_cadr (entry);
      SCM elt_prop_sym = gh_caddr (entry);

      SCM preset = scm_assq(elt_prop_sym, e->mutable_property_alist_);
      if (preset != SCM_BOOL_F)
	continue;
  
      SCM val = get_property (prop_sym);

      if (val == SCM_UNDEFINED)
	;			// Not defined in context.
      else if (gh_apply (type_p, scm_listify (val, SCM_UNDEFINED))
	       == SCM_BOOL_T)	// defined and  right type: do it
	{
	  e->set_elt_property (elt_prop_sym, val);

	  SCM errport = scm_current_error_port ();
	  scm_display (prop_sym, errport);
	  scm_puts (_(" is deprecated. Use\n \\property ").ch_C(), errport);

	  scm_puts (origin->type_str_.ch_C(), errport);
	  scm_puts (".", errport);
	  
	  SCM name = e->get_elt_property ("meta");
	  name = scm_assoc (ly_symbol2scm ("name"), name);
	  scm_display (gh_cdr(name), errport);
	  scm_puts(" \\push #'",errport);
	  scm_display (elt_prop_sym,errport);
	  scm_puts ( " = #",errport);
	  if (gh_string_p (val))
	    scm_puts ("\"", errport);
	  scm_display (val, scm_current_error_port ());
	  if (gh_string_p (val))
	    scm_puts ("\"", errport);
	  scm_puts ("\n", errport);
	}
      else

	/*
	    we don't print a warning if VAL == #f, because we would
	    get lots of warnings when we restore stuff to default, eg.

	    slurDash = #1 [...] slurDash = ##f

	    should not cause "type error: slurDash expects number not
	    boolean"

	*/
	if (val != SCM_BOOL_F)
	  {			// not the right type: error message.
	    SCM errport = scm_current_error_port ();
	    warning (_("Wrong type for property"));
	    scm_display (prop_sym, errport);
	    scm_puts (", type: ", errport);

	    SCM typefunc = scm_eval2 (ly_symbol2scm ("type-name"), SCM_EOL);
	    
	    scm_display (gh_call1 (typefunc, type_p), errport);
	    scm_puts (", value found: ", errport);
	    scm_display (val, errport);
	    scm_puts (" type: ", errport);
	    scm_display (ly_type (val), errport);
	    scm_puts ("\n", errport);
	  }
    }
}

ADD_THIS_TRANSLATOR(Property_engraver);
