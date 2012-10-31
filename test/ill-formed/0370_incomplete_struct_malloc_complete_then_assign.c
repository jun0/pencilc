#include <stdlib.h>
void f ()
{
  struct s;
  struct s *x = (struct s*) malloc (2 * sizeof (int));
  struct s *y = (struct s*) malloc (2 * sizeof (int));
  struct s { double *p; };
  *x = *y;
  x->p = 0;
}
