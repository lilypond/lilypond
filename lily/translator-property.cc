/*   
translator-property.cc --  implement manipulation of

   immutable Grob property lists. 

source file of the GNU LilyPond music typesetter

(c) 2004 Han-Wen Nienhuys <hanwen@xs4all.nl>

 */

#include "translator-group.hh"
#include "warn.hh"
#include "item.hh"
#include "spanner.hh"

/*
  Grob descriptions (ie. alists with layout properties) are
  represented as a (ALIST . BASED-ON) pair, where BASED-ON is the
  alist defined in a parent context. BASED-ON should always be a tail
  of ALIST.
  
  */

/*
  Push or pop (depending on value of VAL) a single entry (ELTPROP . VAL)
  entry from a translator property list by name of PROP
*/


void
execute_pushpop_property (Translator_group * trg,
			  SCM prop, SCM eltprop, SCM val)
{
  if (gh_symbol_p (prop))
    {
      if (val != SCM_UNDEFINED)
	{
	  SCM prev = SCM_EOL;
	  Translator_group * where = trg->where_defined (prop);

	  /*
	    Don't mess with MIDI.
	   */
	  if (!where)
	    return ;
	  if (where != trg)
	    {
	      SCM base = updated_grob_properties (trg, prop);
	      prev = gh_cons (base, base); 
	      trg->internal_set_property (prop, prev);
	    }
	  else
	    prev = trg->internal_get_property (prop);
	  
	  if (!gh_pair_p (prev))
	    {
	      programming_error ("Grob definition should be cons.");
	      return ;
	    }

	  SCM prev_alist = gh_car (prev);
	  
	  if (gh_pair_p (prev_alist) || prev_alist == SCM_EOL)
	    {
	      bool ok = type_check_assignment (eltprop, val, ly_symbol2scm ("backend-type?"));
	      
	      if (ok)
		{
		  gh_set_car_x (prev, scm_acons (eltprop, val, prev_alist));
		}
	    }
	  else
	    {
	      // warning here.
	    }
	}
      else if (trg->where_defined (prop) == trg)
	{
	  SCM prev = trg->internal_get_property (prop);
	  SCM prev_alist = gh_car (prev);
	  SCM daddy = gh_cdr (prev);
	  
	  SCM new_alist = SCM_EOL;
	  SCM *tail = &new_alist;

	  while (prev_alist != daddy)
	    {
	      if (!gh_equal_p (gh_caar (prev_alist), eltprop))
		{
		  *tail = gh_cons (gh_car (prev_alist), daddy);
		  tail = SCM_CDRLOC (*tail);
		}
	      prev_alist = gh_cdr (prev_alist);
	    }

	  if (new_alist == SCM_EOL)
	    trg->unset_property (prop);
	  else
	    trg->internal_set_property (prop, gh_cons (new_alist, daddy));
	}
    }
}

/*
  PRE_INIT_OPS is in the order specified, and hence must be reversed.
 */
void
apply_property_operations (Translator_group*tg, SCM pre_init_ops)
{
  SCM correct_order = scm_reverse (pre_init_ops);
  for (SCM s = correct_order; gh_pair_p (s); s = ly_cdr (s))
    {
      SCM entry = ly_car (s);
      SCM type = ly_car (entry);
      entry = ly_cdr (entry); 
      
      if (type == ly_symbol2scm ("push") || type == ly_symbol2scm ("poppush"))
	{
	  SCM val = ly_cddr (entry);
	  val = gh_pair_p (val) ? ly_car (val) : SCM_UNDEFINED;

	  execute_pushpop_property (tg, ly_car (entry), ly_cadr (entry), val);
	}
      else if (type == ly_symbol2scm ("assign"))
	{
	  tg->internal_set_property (ly_car (entry), ly_cadr (entry));
	}
    }
}

/*
  Return the object alist for SYM, checking if its base in enclosing
  contexts has changed. The alist is updated if necessary. 
   */
SCM
updated_grob_properties (Translator_group* tg, SCM sym)
{
  assert (gh_symbol_p (sym));
  
  tg = tg->where_defined (sym);
  SCM daddy_props
    = (tg->daddy_trans_)
    ? updated_grob_properties (tg->daddy_trans_, sym)
    : SCM_EOL;
  
  SCM props  = tg->internal_get_property (sym);

  if (!gh_pair_p (props))
    {
      programming_error ("grob props not a pair?");
      return SCM_EOL;
    }

  SCM based_on = gh_cdr (props);
  if (based_on == daddy_props)
    {
      return gh_car (props);
    }
  else
    {
      SCM copy = daddy_props;
      SCM * tail = &copy;
      SCM p = gh_car (props);
      while  (p != based_on)
	{
	  *tail = gh_cons (gh_car (p), daddy_props);
	  tail = SCM_CDRLOC (*tail);
	  p = SCM_CDR (p);
	}
      
      scm_set_car_x (props, copy);
      scm_set_cdr_x (props, daddy_props);

      return copy;
    }
}

Item*
make_item_from_properties (Translator_group* tg, SCM x)
{
  SCM props = updated_grob_properties (tg, x);
  return new Item (props);
}

Spanner*
make_spanner_from_properties (Translator_group *tg, SCM x)
{
  SCM props = updated_grob_properties (tg, x);
  return new Spanner (props);
}

