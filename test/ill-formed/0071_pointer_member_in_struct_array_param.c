typedef struct {
  int *x;
  double y;
} s;
void f (s a[static const restrict 5]) {}
