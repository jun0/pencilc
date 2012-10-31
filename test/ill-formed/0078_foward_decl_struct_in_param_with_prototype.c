struct s;
void f (struct s *);
struct s {
  int *p; /* Note in the future we can't immediately reject this definition
           * because might not be used in PENCIL functions/regions.  */
};
void f (struct s *x)
{
  x->p = 0;
}
