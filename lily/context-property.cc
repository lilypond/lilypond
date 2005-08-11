/*
  context-property.cc -- implement manipulation of immutable Grob
  property lists.

  source file of the GNU LilyPond music typesetter

  (c) 2004--2005 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "context.hh"
#include "engraver.hh"
#include "item.hh"
#include "main.hh"
#include "spanner.hh"
#include "warn.hh"
#include "paper-column.hh"

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
execute_pushpop_property (Context *trg,
			  SCM prop, SCM eltprop, SCM val)
{
  SCM prev = SCM_EOL;
  if (scm_is_symbol (prop) && scm_is_symbol (eltprop))
    {
      if (val != SCM_UNDEFINED)
	{
	  Context *where = trg->where_defined (prop, &prev);

	  /*
	    Don't mess with MIDI.
	  */
	  if (!where)
	    return;

	  if (where != trg)
	    {
	      SCM base = updated_grob_properties (trg, prop);
	      prev = scm_cons (base, base);
	      trg->internal_set_property (prop, prev);
	    }

	  if (!scm_is_pair (prev))
	    {
	      programming_error ("Grob definition should be cons");
	      return;
	    }

	  SCM prev_alist = scm_car (prev);

	  if (scm_is_pair (prev_alist) || prev_alist == SCM_EOL)
	    {
	      bool ok = type_check_assignment (eltprop, val, ly_symbol2scm ("backend-type?"));

	      /*
		tack onto alist:
	      */
	      if (ok)
		scm_set_car_x (prev, scm_acons (eltprop, val, prev_alist));
	    }
	  else
	    {
	      // warning here.
	    }
	}
      else if (trg->where_defined (prop, &prev) == trg)
	{
	  SCM prev_alist = scm_car (prev);
	  SCM daddy = scm_cdr (prev);

	  SCM new_alist = SCM_EOL;
	  SCM *tail = &new_alist;

	  while (prev_alist != daddy)
	    {
	      if (ly_is_equal (scm_caar (prev_alist), eltprop))
		{
		  prev_alist = scm_cdr (prev_alist);
		  break;
		}

	      *tail = scm_cons (scm_car (prev_alist), SCM_EOL);
	      tail = SCM_CDRLOC (*tail);
	      prev_alist = scm_cdr (prev_alist);
	    }

	  if (new_alist == SCM_EOL && prev_alist == daddy)
	    trg->unset_property (prop);
	  else
	    {
	      *tail = prev_alist;
	      trg->internal_set_property (prop, scm_cons (new_alist, daddy));
	    }
	}
    }
  else
    {
      warning (_ ("need symbol arguments for \\override and \\revert"));
      if (do_internal_type_checking_global)
	assert (false);
    }
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

      if (type == ly_symbol2scm ("push") || type == ly_symbol2scm ("poppush"))
	{
	  SCM val = scm_cddr (entry);
	  val = scm_is_pair (val) ? scm_car (val) : SCM_UNDEFINED;

	  execute_pushpop_property (tg, scm_car (entry), scm_cadr (entry), val);
	}
      else if (type == ly_symbol2scm ("assign"))
	{
	  tg->internal_set_property (scm_car (entry), scm_cadr (entry));
	}
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
    {
      return scm_car (props);
    }
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

Grob *
make_grob_from_properties (Engraver *tr, SCM symbol, SCM cause, char const *name)
{
  Context *context = tr->context ();

  SCM props = updated_grob_properties (context, symbol);

  Object_key const *key = context->get_grob_key (name);
  Grob *grob = 0;
  
  SCM handle = scm_sloppy_assq (ly_symbol2scm ("meta"), props);
  SCM klass = scm_cdr (scm_sloppy_assq (ly_symbol2scm ("class"), scm_cdr (handle)));
  
  if (klass == ly_symbol2scm ("Item"))
    grob = new Item (props, key);
  else if (klass == ly_symbol2scm ("Spanner"))
    grob = new Spanner (props, key);
  else if (klass == ly_symbol2scm ("Paper_column"))
    grob = new Paper_column (props, key);

  assert (grob);
  dynamic_cast<Engraver *> (tr)->announce_grob (grob, cause);

  return grob;
}

Item *
make_item_from_properties (Engraver *tr, SCM x, SCM cause, char const *name)
{
  Item *it = dynamic_cast<Item*> (make_grob_from_properties (tr, x, cause, name));
  assert (it);
  return it;
}

Paper_column *
make_paper_column_from_properties (Engraver *tr, SCM x, char const *name)
{
  return dynamic_cast<Paper_column*> (make_grob_from_properties (tr, x, SCM_EOL, name));
}


Spanner *
make_spanner_from_properties (Engraver *tr, SCM x, SCM cause, char const *name)
{
  Spanner *sp = dynamic_cast<Spanner*> (make_grob_from_properties (tr, x, cause, name));
  assert (sp);
  return sp;
}
