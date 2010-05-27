#include "context.hh"
#include "grob.hh"


/*
  Drop symbol from the list alist..alist_end.
 */
SCM
evict_from_alist (SCM symbol, SCM alist, SCM alist_end)
{
  SCM new_alist = SCM_EOL;
  SCM *tail = &new_alist;

  while (alist != alist_end)
    {
      if (ly_is_equal (scm_caar (alist), symbol))
	{
	  alist = scm_cdr (alist);
	  break;
	}

      *tail = scm_cons (scm_car (alist), SCM_EOL);
      tail = SCM_CDRLOC (*tail);
      alist = scm_cdr (alist);
    }

  *tail = alist;
  return new_alist;
}

/*
  PROP_PATH should be big-to-small ordering
 */
SCM
nested_property_alist (SCM alist, SCM prop_path, SCM value)
{
  SCM new_value = SCM_BOOL_F;
  if (scm_is_pair (scm_cdr (prop_path)))
    {
      SCM sub_alist = ly_assoc_get (scm_car (prop_path), alist, SCM_EOL);
      new_value = nested_property_alist (sub_alist, scm_cdr (prop_path), value);
    }
  else
    {
      new_value = value;
    }

  return scm_acons (scm_car (prop_path), new_value, alist);
}

/*
  Recursively purge alist of prop_path:

  revert ((sym, val) : L, [sym]) = L
  revert ((sym, val) : L, sym : props) =
    (sym, revert (val, rest-props)) ++ L
  revert ((sym, val) : L, p ++ rest-props) =
    (sym, val) : revert (L, p ++ rest-props)

 */
SCM
nested_property_revert_alist (SCM alist, SCM prop_path)
{
  int copy_count = 0;
  bool drop = false;
  assert(scm_is_pair (prop_path));

  SCM wanted_sym = scm_car (prop_path);

  SCM new_list = SCM_EOL;
  SCM *tail = &new_list;
  for (SCM s = alist; scm_is_pair (s); s = scm_cdr (s))
    {
      SCM sub_sym = scm_caar (s);
      SCM old_val = scm_cdar (s);
      drop = false;

      if (sub_sym == wanted_sym)
	{
	  if (scm_is_pair (scm_cdr (prop_path)))
	    {
	      SCM new_val = nested_property_revert_alist (old_val, scm_cdr (prop_path));

	      /* nothing changed: drop newly constructed list. */
	      if (old_val == new_val)
		return alist;

	      *tail = scm_acons (sub_sym, new_val, SCM_EOL);
	      tail = SCM_CDRLOC(*tail);
              *tail = scm_cdr (s);
              return new_list;
	    }
	  else
	    {
              /* old value should be dropped only if we have another copy of it in the alist */
              copy_count++;
              /*
                Only drop the first instance found.
                the overridden value is always the first
                if this was the only copy, we will return
                the original list anyways so it is not relevant
                if we drop this pair
              */
              if (copy_count == 1)
                drop = true;
	    }
	  /* we now iterate over every item */
	}
      /*
        Make a new list with every item
        except for the eventual dropped one
      */
      if (!drop)
        {
          *tail = scm_acons (sub_sym, old_val, SCM_EOL);
          tail = SCM_CDRLOC (*tail);
        }
    }

  /*
    If we find more than one copy of the property
    push the new list, else it means we are trying to
    revert the original value
  */
  if (copy_count > 1)
    return new_list;
  else
    return alist;
}


void
set_nested_property (Grob *me, SCM big_to_small, SCM value)
{
  SCM alist = me->get_property (scm_car (big_to_small));

  alist = nested_property_alist (alist, scm_cdr (big_to_small), value);

  me->set_property (scm_car (big_to_small), alist);
}

