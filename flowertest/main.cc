#include "flower-test.hh"
#include "varray.hh"

Array< fptr > *test_arr_p;

void reg_test (fptr f)
{
 if (!test_arr_p)
  test_arr_p = new Array<fptr>;
 test_arr_p->push (f);
}

int
main ()
{
 if (test_arr_p)
   {
     for (int i=0; i < test_arr_p->size (); i++)
       (*test_arr_p)[i] ();
   }
}
