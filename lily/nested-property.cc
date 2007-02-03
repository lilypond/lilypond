#include "context.hh"
#include "grob.hh"

SCM
evict_from_alist (SCM symbol,
		  SCM alist,
		  SCM alist_end)
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

SCM 
nested_property_revert_alist (SCM alist, SCM prop_path)
{
  SCM new_sub_alist = SCM_EOL;
  SCM sym = scm_car (prop_path);
  if (scm_is_pair (scm_cdr (prop_path)))
    {
      SCM sub_alist = ly_assoc_get (sym, alist, SCM_EOL);
      new_sub_alist = nested_property_revert_alist (sub_alist, scm_cdr (prop_path));
    }
  else
    {
      new_sub_alist = evict_from_alist (sym, alist, SCM_EOL);
    }

  return scm_acons (sym, new_sub_alist, alist);
}


void
set_nested_property (Grob *me, SCM big_to_small, SCM value)
{
  SCM alist = me->get_property (scm_car (big_to_small));

  alist = nested_property_alist (alist, scm_cdr (big_to_small), value);
  
  me->set_property (scm_car (big_to_small),
		    alist);
}

