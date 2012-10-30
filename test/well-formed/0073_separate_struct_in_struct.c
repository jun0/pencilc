typedef struct {
  char s[100];
} str;
typedef struct {
  double x, y;
  str label;
} labeled_point;

void f (labeled_point p) {}
