void f ()
{
  struct s;
  struct s *x = 0;
  struct s { double *p; };
  x->p = 0;
}
