#include "string.hh"
#include "array.hh"
#include "dictionary.hh"


// Note: assumes long is at least 32 bits.
const unsigned long my_prime_list[] = 
{
  53,         97,         193,       389,       769,
  1543,       3079,       6151,      12289,     24593,
  49157,      98317,      196613,    393241,    786433,
  1572869,    3145739,    6291469,   12582917,  25165843,
  50331653,   100663319,  201326611, 402653189u, 805306457u, 
  1610612741u, 3221225473u, 4294967291u
};

unsigned long prime_list (int idx)
{
  return my_prime_list [idx];
}

unsigned int hash (String s)
{
  const char* str = s.ch_C ();
  unsigned int result = 0;
  while (1) {
    char c = *str++;
    if (c == 0) break;
      result += (result<<3) + c;
  }
  return result;
}

