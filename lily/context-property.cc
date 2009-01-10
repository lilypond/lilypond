/*
  context-property.cc -- implement manipulation of immutable Grob
  property lists.

  source file of the GNU LilyPond music typesetter

  (c) 2004--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "context.hh"
#include "engraver.hh"
#include "international.hh"
#include "item.hh"
#include "main.hh"
#include "simple-closure.hh"
#include "spanner.hh"
#include "warn.hh"

/*
  like execute_general_pushpop_property(), but typecheck
  grob_property_path and context_property.
*/
void
general_pushpop_property (Context *context,
			  SCM context_property,
			  SCM grob_property_path,
			  SCM new_value)
{
  if (!scm_is_symbol (context_property)
      || !scm_is_symbol (scm_car (grob_property_path)))
    {
      warning (_ ("need symbol arguments for \\override and \\revert"));
      if (do_internal_type_checking_global)
	assert (false);
    }

  sloppy_general_pushpop_property (context, context_property,
				    grob_property_path, new_value);
}


/*
  Grob descriptions (ie. alists with layout properties) are
  represented as a (ALIST . BASED-ON) pair, where BASED-ON is the
  alist defined in a parent context. BASED-ON should always be a tail
  of ALIST.

  Push or pop (depending on value of VAL) a single entry entry from a
  translator property list by name of PROP.  GROB_PROPERTY_PATH
  indicates nested alists, eg. '(beamed-stem-lengths details)
  
*/
void
execute_override_property (Context *context,
			   SCM context_property,
			   SCM grob_property_path,
			   SCM new_value)
{
  SCM current_context_val = SCM_EOL;
  
  Context *where = context->where_defined (context_property,
					   &current_context_val);

  /*
    Don't mess with MIDI.
  */
  if (!where)
    return;

  if (where != context)
    {
      SCM base = updated_grob_properties (context, context_property);
      current_context_val = scm_cons (base, base);
      context->set_property (context_property, current_context_val);
    }

  if (!scm_is_pair (current_context_val))
    {
      programming_error ("Grob definition should be cons");
      return;
    }

  SCM target_alist = scm_car (current_context_val);

  SCM symbol = scm_car (grob_property_path);
  if (scm_is_pair (scm_cdr (grob_property_path)))
    {
      new_value = nested_property_alist (ly_assoc_get (symbol, target_alist, 
						       SCM_EOL),
					 scm_cdr (grob_property_path),
					 new_value);
    }

  /* it's tempting to replace the head of the list if it's the same
   property. However, we have to keep this info around, in case we have to
   \revert back to it.
  */
  target_alist = scm_acons (symbol, new_value, target_alist);

  bool ok = true;
  if (!ly_is_procedure (new_value)
      && !is_simple_closure (new_value))
    ok = type_check_assignment (symbol, new_value,
				ly_symbol2scm ("backend-type?"));

  /*
    tack onto alist.  We can use set_car, since
    updated_grob_properties () in child contexts will check
    for changes in the car.
  */
  if (ok)
    {
      scm_set_car_x (current_context_val, target_alist);
    }
}

/*
  do a pop (indicated by new_value==SCM_UNDEFINED) or push
 */
void
sloppy_general_pushpop_property (Context *context,
				 SCM context_property,
				 SCM grob_property_path,
				 SCM new_value)
{
  if (new_value == SCM_UNDEFINED)
    execute_revert_property (context, context_property,
			     grob_property_path);
  else
    execute_override_property (context, context_property,
			       grob_property_path,
			       new_value);
}

/*
  Revert the property given by property_path.
*/
void
execute_revert_property (Context *context,
			 SCM context_property,
			 SCM grob_property_path)
{
  SCM current_context_val = SCM_EOL;
  if (context->where_defined (context_property, &current_context_val)
      == context)
    {
      SCM current_alist = scm_car (current_context_val);
      SCM daddy = scm_cdr (current_context_val);

      if (!scm_is_pair (grob_property_path)
	  || !scm_is_symbol (scm_car (grob_property_path)))
	{
	  programming_error ("Grob property path should be list of symbols.");
	  return;
	}
      
      SCM symbol = scm_car (grob_property_path);
      if (scm_is_pair (scm_cdr (grob_property_path)))
	{
	  SCM current_sub_alist = ly_assoc_get (symbol, current_alist, SCM_EOL);
	  SCM new_val
	    = nested_property_revert_alist (current_sub_alist,
					    scm_cdr (grob_property_path));
	    
	  if (scm_is_pair (current_alist)
	      && scm_caar (current_alist) == symbol
	      && current_alist != daddy)
	    current_alist = scm_cdr (current_alist);

	  current_alist = scm_acons (symbol, new_val, current_alist);
	  scm_set_car_x (current_context_val, current_alist);
	}
      else
	{
	  SCM new_alist = evict_from_alist (symbol, current_alist, daddy);
	  
	  if (new_alist == daddy)
	    context->unset_property (context_property);
	  else
	    context->set_property (context_property,
				   scm_cons (new_alist, daddy));
	}
    }
}
/*
  Convenience: a push/pop grob property using a single grob_property
  as argument.
*/
void
execute_pushpop_property (Context *context,
			  SCM context_property,
			  SCM grob_property,
			  SCM new_value)
{
  general_pushpop_property (context, context_property,
			    scm_list_1 (grob_property),
			    new_value);
}
  
/*
  PRE_INIT_OPS is in the order specified, and hence must be reversed.
*/
void
apply_property_operations (Context *tg, SCM pre_init_ops)
{
  SCM correct_order = scm_reverse (pre_init_ops);
  for (SCM s = correct_order; scm_is_pair (s); s = scm_cdr (s))
    {
      SCM entry = scm_car (s);
      SCM type = scm_car (entry);
      entry = scm_cdr (entry);

      if (type == ly_symbol2scm ("push"))
	{
	  SCM context_prop = scm_car (entry);
	  SCM val = scm_cadr (entry);
	  SCM grob_prop_path = scm_cddr (entry);
	  sloppy_general_pushpop_property (tg, context_prop, grob_prop_path, val);
	}
      else if (type == ly_symbol2scm ("pop"))
	{
	  SCM context_prop = scm_car (entry);
	  SCM val = SCM_UNDEFINED;
	  SCM grob_prop_path = scm_cdr (entry);
	  sloppy_general_pushpop_property (tg, context_prop, grob_prop_path, val);
	}
      else if (type == ly_symbol2scm ("assign"))
	tg->set_property (scm_car (entry), scm_cadr (entry));
    }
}

/*
  Return the object alist for SYM, checking if its base in enclosing
  contexts has changed. The alist is updated if necessary.
*/
SCM
updated_grob_properties (Context *tg, SCM sym)
{
  assert (scm_is_symbol (sym));

  SCM props;
  tg = tg->where_defined (sym, &props);
  if (!tg)
    return SCM_EOL;

  SCM daddy_props
    = (tg->get_parent_context ())
    ? updated_grob_properties (tg->get_parent_context (), sym)
    : SCM_EOL;

  if (!scm_is_pair (props))
    {
      programming_error ("grob props not a pair?");
      return SCM_EOL;
    }

  SCM based_on = scm_cdr (props);
  if (based_on == daddy_props)
    return scm_car (props);
  else
    {
      SCM copy = daddy_props;
      SCM *tail = &copy;
      SCM p = scm_car (props);
      while (p != based_on)
	{
	  *tail = scm_cons (scm_car (p), daddy_props);
	  tail = SCM_CDRLOC (*tail);
	  p = scm_cdr (p);
	}

      scm_set_car_x (props, copy);
      scm_set_cdr_x (props, daddy_props);

      return copy;
    }
}
