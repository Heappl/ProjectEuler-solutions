#include <iostream>
#include <stdlib.h>

unsigned reverse(unsigned x)
{
   unsigned out = 0;
   while (x) {
      out = out * 10 + x % 10;
      x /= 10;
   }
   return out;
}

bool reversible(unsigned x)
{
   while (x) {
      if (!((x % 10) & 1)) return false;
      x /= 10;
   }
   return true;
}

unsigned countReversible(unsigned max)
{
   unsigned count = 0;
   for (unsigned i = 12; i < max; ++i) {
      if (i % 10 == 0) continue;
      if (reversible(i + reverse(i))) ++count;
   }
   return count;
}


int main(int argc, char** argv)
{
   std::cout << countReversible(atoi(argv[1])) << std::endl;
   return 0;
}

