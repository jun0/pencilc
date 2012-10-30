typedef struct {
  int (*f) (void);
  double y;
} s;
void f (s a[static const restrict 5]) {}
