struct s {
  union u {
    int x;
  } x;
};

struct s f () {
  struct s ret;
  ret.x.x = 0;
  return ret;
}
