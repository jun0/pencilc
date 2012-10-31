typedef struct {
  union {
    int x;
    double y;
  };
} s;

void f (struct s x) {}
