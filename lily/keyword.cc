/*
  keyword.cc -- keywords and identifiers
 */
#include <string.h>
#include <stdlib.h>
#include "keyword.hh"


/* for qsort */
int tabcmp (Keyword_ent  const &p1, Keyword_ent const &p2)
{
  return strcmp (p1.name_, p2.name_);
}

Keyword_table::Keyword_table (Keyword_ent *tab)
{
  while (tab->name_)
    {
      table_.push (*tab++);
    }

  table_.sort (tabcmp);
}

int
Keyword_table::lookup (char const *s) const
{
  Keyword_ent e ;
  e.name_ =  s;
  int idx = binary_search (table_, e, tabcmp);
  if (idx >= 0)
    return table_[idx].tokcode_;
  else
    return -1;
}
