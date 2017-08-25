
#include <gtk/gtk.h>
#include <math.h>

char cPositionOf(int rule, int row, int col);

typedef char (*position_t)(int, int, int);

position_t position_of = NULL;

GtkWidget* window;
GtkWidget* grid;

GtkWidget* box;
GtkWidget* rule_label;
GtkWidget* rule;
GtkWidget* size_label;
GtkWidget* size;

GtkWidget* area;

static void value_changed(GtkRange* range, gpointer user_data) {
  if (gtk_widget_is_visible(window))
    gdk_window_invalidate_rect(gtk_widget_get_window(window), NULL, TRUE);
}

static gboolean draw_callback(GtkWidget* widget, cairo_t* cr, gpointer user_data) {

  guint width  = gtk_widget_get_allocated_width(widget);
  guint height = gtk_widget_get_allocated_height(widget);

  cairo_rectangle(cr, 0, 0, width, height);
  cairo_set_source_rgb(cr, 1.0, 1.0, 1.0);
  cairo_fill(cr);

  double square_width = gtk_range_get_value(GTK_RANGE(size));
  double square_height = square_width;

  double row_count = height / square_height;
  double col_count = width / square_width;

  for (int r = 0; r < row_count; r++) {
    for (int c = 0; c < col_count; c++) {
      int i = floor(c - col_count / 2);
      cairo_rectangle(cr, c * square_width, r * square_height, square_width, square_height);
      double x = (position_of((int)gtk_range_get_value(GTK_RANGE(rule)), r, i) ? 0 : 1);
      cairo_set_source_rgb(cr, x, x, x);
      cairo_fill(cr);
    }
  }

  return FALSE;
}

static void activate(GtkApplication* app, gpointer user_data) {

  window = gtk_application_window_new(app);
  gtk_window_set_title(GTK_WINDOW(window), "Cellular");
  gtk_window_set_default_size(GTK_WINDOW(window), 400, 400);

  grid = gtk_grid_new();
  gtk_container_add(GTK_CONTAINER(window), grid);

  box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
  gtk_widget_set_size_request(box, 200, 400);
  gtk_grid_attach(GTK_GRID(grid), box, 0, 0, 1, 1);

  rule_label = gtk_label_new("Rule:");
  gtk_box_pack_start(GTK_BOX(box), rule_label, FALSE, FALSE, 0);

  rule = gtk_scale_new_with_range(GTK_ORIENTATION_HORIZONTAL,
                                   0,
                                   255,
                                   1);
  g_signal_connect(G_OBJECT(rule), "value-changed",
                   G_CALLBACK(value_changed), NULL);
  gtk_range_set_value(GTK_RANGE(rule), 30);
  gtk_box_pack_start(GTK_BOX(box), rule, FALSE, FALSE, 0);

  size_label = gtk_label_new("Cell Size:");
  gtk_box_pack_start(GTK_BOX(box), size_label, FALSE, FALSE, 0);

  size = gtk_scale_new_with_range(GTK_ORIENTATION_HORIZONTAL,
                                   5.0,
                                   20.0,
                                   0.1);
  g_signal_connect(G_OBJECT(size), "value-changed",
                   G_CALLBACK(value_changed), NULL);
  gtk_range_set_value(GTK_RANGE(size), 15.0);
  gtk_box_pack_start(GTK_BOX(box), size, FALSE, FALSE, 0);

  area = gtk_drawing_area_new();
  gtk_widget_set_size_request(area, 200, 400);
  g_signal_connect(G_OBJECT(area), "draw",
                   G_CALLBACK(draw_callback), NULL);
  gtk_grid_attach(GTK_GRID(grid), area, 1, 0, 1, 1);

  g_object_set(grid, "expand", TRUE, NULL);
  g_object_set(box, "expand", TRUE, NULL);
  g_object_set(area, "expand", TRUE, NULL);

  gtk_widget_show_all(window);
}

GtkApplication* init_application(position_t func) {
  GtkApplication* app = gtk_application_new("com.mercerenies.gtk.cellular",
                                            G_APPLICATION_FLAGS_NONE);
  g_signal_connect(app, "activate", G_CALLBACK(activate), NULL);
  position_of = func;
  return app;
}

int run_application(GtkApplication* app, int argc, char** argv) {
  return g_application_run(G_APPLICATION(app), argc, argv);
}

void free_application(GtkApplication* app) {
  g_object_unref(app);
}
