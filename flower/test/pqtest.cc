#include "flower-test.hh"
#include "pqueue.hh"
#include <stdlib.h>

int compare (int i, int j)
{
  return i-j;
}

void
pqtest ()
{
  PQueue<int> pq;

  for  (int i=0; i < 10; i++) {
    int r = rand ()/10000;
    pq.insert (r);
    cout << "adding: " << r<< endl;
  }
  while  (pq.size ()) {
    cout << "getting : "<< flush;
    cout << pq.get () << "\n";
  }
}

ADD_TEST (pqtest);
