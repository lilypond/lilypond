#include <stdio.h>
#include <iostream.h>
#include "dictionary-iter.hh"



main ()
{
  Dictionary<String> *dict = new Dictionary<String>;
  
  char s[1000];

  dict->elem("foo") = "bar";
  dict->elem("bla") = "ba";
  dict->elem("blo") = "bar";
  
  while (gets (s))
    {
      String str (s);

      int l = str.length_i ();

      dict->elem (str.left_str (l/2)) = str.right_str (l/2);
    }

  int i=1000;
  while (i--)
    {
      Dictionary<String> *dict2=new Dictionary<String> (*dict);
      delete dict;
      dict = dict2;
    }

  for (Dictionary_iter<String> i (*dict); i.ok (); i++)
    {
      cout << i.key () << " == " << i.val () << endl;
      cout << "elem_b: " << dict->elem_b (i.key ()) << ", key " << i.key () << " val " << dict->elem (i.key ()) <<endl;
    }
}
