#include "newkey.hh"

Newkey::Newkey ()
{
  clear();
}

void
Newkey::clear()
{
  key_alist_ = SCM_EOL;
}

void
Newkey::set (int n, int a)
{
  set_scm (gh_int2scm (n), gh_int2scm (a));
}

void
Newkey::set (int o, int n, int a)
{
  set_scm (gh_cons (gh_int2scm (o),gh_int2scm(n)), gh_int2scm (a));
}

void
Newkey::set_scm (SCM k, SCM v)
{
  key_alist_
    = scm_assoc_set_x (key_alist_, k, v);  
}

int
Newkey::get (int o, int n)
{
  SCM r = scm_assoc (gh_cons (gh_int2scm (o), gh_int2scm (n)), key_alist_);
  return r == SCM_BOOL_F ? get (n) : gh_cdr (r);
}

int
Newkey::get (int n)
{
  SCM r = scm_assoc (gh_int2scm (n), key_alist_);
  return r == SCM_BOOL_F ? 0: gh_cdr (r);
}
