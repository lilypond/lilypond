/*
  flower-test.hh -- declare 

  source file of the Flower Library

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef FLOWER_TEST_HH
#define FLOWER_TEST_HH
#include <iostream.h>

#define ADD_TEST(f) \
struct f ## Init {\
    f ## Init () { reg_test(f); }\
} f ## init\

typedef void (*fptr)(void);

void reg_test (fptr f);

#endif // FLOWER_TEST_HH
/*
  flower-test.hh -- declare 

  source file of the Flower Library

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef FLOWER_TEST_HH
#define FLOWER_TEST_HH
#include <iostream.h>

#define ADD_TEST(f) \
struct f ## Init {\
    f ## Init () { reg_test(f); }\
} f ## init\

typedef void (*fptr)(void);

void reg_test (fptr f);

#endif // FLOWER_TEST_HH
