struct s;
struct s g ();
struct s f ()
{
  return g ();
}
struct s { int *p; };
