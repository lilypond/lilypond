/*   
  property-engraver.cc --  implement Property engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "lily-guile.hh"
#include "engraver.hh"
#include "dictionary.hh"
#include "grob.hh"
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
  void apply_properties (SCM, Grob*, Translator_group *origin);

protected:
  virtual void acknowledge_grob (Grob_info ei);
  virtual void initialize ();
  virtual void finalize ();
public:
  ~Property_engraver ();
  Property_engraver ();
  VIRTUAL_COPY_CONS (Translator);
};



Property_engraver::Property_engraver ()
{
  prop_dict_ = 0;
}
void
Property_engraver::finalize ()
{
  
}

Property_engraver::~Property_engraver ()
{
  if (prop_dict_)
    scm_gc_unprotect_object (prop_dict_->self_scm ());
}

void
Property_engraver::initialize ()
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
Property_engraver::acknowledge_grob (Grob_info i)
{
 SCM ifs = i.elem_l_->get_grob_property ("interfaces");
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
Property_engraver::apply_properties (SCM p, Grob *e, Translator_group*origin)
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

      SCM preset = scm_assq (elt_prop_sym, e->mutable_property_alist_);
      if (preset != SCM_BOOL_F)
	continue;
  
      SCM val = get_property (prop_sym);

      if (val == SCM_EOL)
	;			// Not defined in context.
      else if (gh_apply (type_p, scm_list_n (val, SCM_UNDEFINED))
	       == SCM_BOOL_T)	// defined and  right type: do it
	{
	  e->set_grob_property (elt_prop_sym, val);

	  SCM meta = e->get_grob_property ("meta");
	  SCM name = scm_assoc (ly_symbol2scm ("name"), meta);
	  warning (_f ("`%s' is deprecated.  Use\n \\property %s.%s \\override #'%s = #%s",
		       ly_symbol2string (prop_sym).ch_C (),
		       origin->type_str_.ch_C (),
		       ly_scm2string (gh_cdr (name)).ch_C (),
		       ly_symbol2string (elt_prop_sym).ch_C (),
		       ly_scm2string (ly_write2scm (val)).ch_C ()));
	}
      else

	/*
	    we don't print a warning if VAL == (), because we would
	    get lots of warnings when we restore stuff to default, eg.

	    slurDash = #1 [...] slurDash = ()

	    should not cause "type error: slurDash expects number not
	    boolean

	*/
	if (val != SCM_EOL)
	  {			// not the right type: error message.
	    SCM errport = scm_current_error_port ();
	    SCM typefunc = scm_primitive_eval (ly_symbol2scm ("type-name"));
	    SCM type_name = gh_call1 (typefunc, type_p);
	    warning (_f ("Wrong type for property: %s, type: %s, value found: %s, type: %s",
			 ly_symbol2string (prop_sym).ch_C (),
			 ly_scm2string (type_name).ch_C (),
			 ly_scm2string (ly_write2scm (val)).ch_C (),
			 ly_scm2string (ly_type (val)).ch_C ()));
	    scm_puts ("\n", errport);
	  }
    }
}

ADD_THIS_TRANSLATOR (Property_engraver);
