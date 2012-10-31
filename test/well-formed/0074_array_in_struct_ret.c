struct s {
  int a[1];
  double x;
};

struct s f () {
  struct s ret;
  ret.a[0] = 0;
  ret.x = 0;
  return ret;
}
