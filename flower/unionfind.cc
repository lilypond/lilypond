#include "unionfind.hh"
/*
  see a book on data structures
  */

Union_find::Union_find (int n)
{
  classes_.set_size (n);

  for (int i=0; i < n; i++) 
    {
      classes_[i] = i;
    }
}

int
Union_find::find (int i)
{
  int rep = i;
  while (classes_[rep] != rep)
    rep = classes_[rep];
  while (classes_[i] != rep) 
    {
      int next =classes_[i];
      classes_[i] = rep;
      i = next;
    }
  return rep;
}

void
Union_find::connect (int i, int j)
{
  i = find (i);
  j = find (j);
  classes_[i] = j;    
}
