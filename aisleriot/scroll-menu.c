/* Scroll menu: a scrolling menu (if it's beyond screen size)
 * (C) 2000  Eazel, Inc.
 *
 * Authors:  George Lebl
 */

#include <gtk/gtk.h>

#include "scroll-menu.h"

static void scroll_menu_class_init	(ScrollMenuClass  *klass);
static void scroll_menu_init		(ScrollMenu       *self);
static void scroll_menu_size_request    (GtkWidget        *widget,
					 GtkRequisition   *requisition);
static void scroll_menu_size_allocate   (GtkWidget        *widget,
					 GtkAllocation    *allocation);
static void scroll_menu_map             (GtkWidget        *widget);
static void scroll_menu_unmap           (GtkWidget        *widget);
static void scroll_menu_destroy         (GtkObject        *object);
static void scroll_menu_move_current    (GtkMenuShell     *menu_shell,
					 GtkMenuDirectionType direction);
static void scroll_menu_remove          (GtkContainer     *container,
					 GtkWidget        *widget);
static gboolean scroll_menu_enter_notify(GtkWidget        *widget,
					 GdkEventCrossing *event);
static gboolean scroll_menu_leave_notify(GtkWidget        *widget,
					 GdkEventCrossing *event);

static GtkMenuClass *parent_class = NULL;

enum {
	SCROLL_UP,
	SCROLL_DOWN
};

#define SCROLLER_HEIGHT 15
#define SCROLL_BY 15
#define SCROLL_TIMEOUT 150
#define ARROW_PAD 3

GtkType
scroll_menu_get_type (void)
{
	static GtkType scroll_menu_type = 0;

	if (scroll_menu_type == 0) {
		GtkType menu_type;

		GtkTypeInfo scroll_menu_info = {
			"ScrollMenu",
			sizeof (ScrollMenu),
			sizeof (ScrollMenuClass),
			(GtkClassInitFunc) scroll_menu_class_init,
			(GtkObjectInitFunc) scroll_menu_init,
			(GtkArgSetFunc) NULL,
			(GtkArgGetFunc) NULL,
			NULL
		};

		menu_type = gtk_menu_get_type ();

		scroll_menu_type = gtk_type_unique (menu_type,
						    &scroll_menu_info);
		parent_class = gtk_type_class (menu_type);
	}

	return scroll_menu_type;
}

static void
scroll_menu_class_init (ScrollMenuClass *klass)
{
	GtkObjectClass *object_class = (GtkObjectClass*) klass;
	GtkWidgetClass *widget_class = (GtkWidgetClass*) klass;
	GtkContainerClass *container_class = (GtkContainerClass*) klass;
	GtkMenuShellClass *menushell_class = (GtkMenuShellClass*) klass;
	
	widget_class->size_request = scroll_menu_size_request;
	widget_class->size_allocate = scroll_menu_size_allocate;
	widget_class->map = scroll_menu_map;
	widget_class->unmap = scroll_menu_unmap;
	widget_class->enter_notify_event = scroll_menu_enter_notify;
	widget_class->leave_notify_event = scroll_menu_leave_notify;

	menushell_class->move_current = scroll_menu_move_current;

	container_class->remove = scroll_menu_remove;

	object_class->destroy = scroll_menu_destroy;
}

static void
scroll_by (ScrollMenu *self, int move)
{
	GtkMenuShell *menu_shell = GTK_MENU_SHELL (self);
	GtkAllocation allocation;
	GList *children;
	GtkWidget *child;

	if (self->offset + move < 0) {
		move = - self->offset;
	} else if (self->offset + move > self->max_offset) {
		move = self->max_offset - self->offset;
	}

	if (move == 0)
		return;

	self->offset += move;

	children = menu_shell->children;
	while (children) {
		child = children->data;
		children = children->next;

		if (GTK_WIDGET_VISIBLE (child)) {
			allocation = child->allocation;
			allocation.y -= move;

			gtk_widget_size_allocate (child, &allocation);
			gtk_widget_queue_draw (child);
		}
	}
}

static gboolean
scroll_timeout (gpointer data)
{
	ScrollMenu *self = SCROLL_MENU (data);
	GtkMenuShell *menu_shell = GTK_MENU_SHELL (self);

	if (self->scroll_by == 0 ||
	    menu_shell->children == NULL) {
		return TRUE;
	}

	scroll_by (self, self->scroll_by);

	return TRUE;
}

static void
scroll_draw (GtkWidget *scroll, gboolean active, int direction)
{
	GtkArrowType arrow_type;

	if( ! GTK_WIDGET_DRAWABLE(scroll))
		return;

	gdk_window_clear_area (scroll->window,
			       0, 0,
			       scroll->allocation.width,
			       scroll->allocation.height);

	gtk_draw_shadow (scroll->style,
			 scroll->window,
			 scroll->state,
			 active ? GTK_SHADOW_IN : GTK_SHADOW_OUT,
			 0, 0,
			 scroll->allocation.width,
			 scroll->allocation.height);

	if (direction == SCROLL_UP) {
		arrow_type = GTK_ARROW_UP;
	} else {
		arrow_type = GTK_ARROW_DOWN;
	}

	gtk_draw_arrow (scroll->style,
			scroll->window,
			scroll->state,
			active ? GTK_SHADOW_IN : GTK_SHADOW_OUT,
			arrow_type,
			TRUE,
			(scroll->allocation.width / 2) - 
			  SCROLLER_HEIGHT / 2 - ARROW_PAD,
			ARROW_PAD,
			SCROLLER_HEIGHT - ARROW_PAD * 2,
			SCROLLER_HEIGHT - ARROW_PAD * 2);
}

static gboolean
scroll_expose (GtkWidget *scroll, GdkEventExpose *event, gpointer data)
{
	ScrollMenu *self = SCROLL_MENU (gtk_object_get_user_data (GTK_OBJECT (scroll)));
	int direction = GPOINTER_TO_INT (data);
	gboolean active = FALSE;

	if (direction == SCROLL_UP) {
		active = self->in_up;
	} else {
		active = self->in_down;
	}

	scroll_draw (scroll, active, direction);
	
	return FALSE;
}

static void
scroll_realize (GtkWidget *scroll, gpointer data)
{
	ScrollMenu *self = SCROLL_MENU (gtk_object_get_user_data (GTK_OBJECT (scroll)));
	int direction = GPOINTER_TO_INT (data);
	gboolean active = FALSE;

	if (direction == SCROLL_UP) {
		active = self->in_up;
	} else {
		active = self->in_down;
	}

	scroll_draw (scroll, active, direction);
}

static gboolean
scroll_enter_notify (GtkWidget *scroll, GdkEventCrossing *event, gpointer data)
{
	ScrollMenu *self = SCROLL_MENU (gtk_object_get_user_data (GTK_OBJECT (scroll)));
	int direction = GPOINTER_TO_INT (data);

	if (direction == SCROLL_UP) {
		self->in_up = TRUE;
		self->scroll_by = -(SCROLL_BY);
	} else {
		self->in_down = TRUE;
		self->scroll_by = SCROLL_BY;
	}

	gtk_widget_queue_draw (scroll);

	if (self->scroll_timeout == 0) {
		self->scroll_timeout = gtk_timeout_add (SCROLL_TIMEOUT,
							scroll_timeout,
							self);
	}
	
	return FALSE;
}

static gboolean
scroll_leave_notify (GtkWidget *scroll, GdkEventCrossing *event, gpointer data)
{
	ScrollMenu *self = SCROLL_MENU (gtk_object_get_user_data (GTK_OBJECT (scroll)));
	int direction = GPOINTER_TO_INT (data);

	self->scroll_by = 0;

	if (direction == SCROLL_UP) {
		self->in_up = FALSE;
	} else {
		self->in_down = FALSE;
	}

	gtk_widget_queue_draw (scroll);

	if (self->scroll_timeout != 0) {
		gtk_timeout_remove (self->scroll_timeout);
		self->scroll_timeout = 0;
	}
	
	return FALSE;
}

static GtkWidget *
make_scroller (ScrollMenu *self, int direction)
{
	GtkWidget *scroll;

	scroll = gtk_drawing_area_new ();
	gtk_widget_set_events(scroll,
			      gtk_widget_get_events(scroll) |
			      GDK_BUTTON_PRESS_MASK |
			      GDK_ENTER_NOTIFY_MASK |
			      GDK_LEAVE_NOTIFY_MASK);
	gtk_widget_set_usize (scroll, 0, SCROLLER_HEIGHT);
	gtk_widget_show (scroll);

	gtk_object_set_user_data (GTK_OBJECT (scroll), self);

	gtk_signal_connect (GTK_OBJECT (scroll), "enter_notify_event",
			    GTK_SIGNAL_FUNC (scroll_enter_notify),
			    GINT_TO_POINTER (direction));
	gtk_signal_connect (GTK_OBJECT (scroll), "leave_notify_event",
			    GTK_SIGNAL_FUNC (scroll_leave_notify),
			    GINT_TO_POINTER (direction));

	gtk_signal_connect_after(GTK_OBJECT(scroll), "realize",
				 GTK_SIGNAL_FUNC(scroll_realize),
				 GINT_TO_POINTER (direction));
	gtk_signal_connect(GTK_OBJECT(scroll), "expose_event",
			   GTK_SIGNAL_FUNC(scroll_expose),
			   GINT_TO_POINTER (direction));

	return scroll;
}

static void
scroll_menu_init (ScrollMenu *self)
{
	self->up_scroll = make_scroller (self, SCROLL_UP);
	gtk_widget_ref (self->up_scroll);
	gtk_object_sink (GTK_OBJECT (self->up_scroll));
	gtk_widget_set_parent (self->up_scroll, GTK_WIDGET(self));

	self->down_scroll = make_scroller (self, SCROLL_DOWN);
	gtk_widget_ref (self->down_scroll);
	gtk_object_sink (GTK_OBJECT (self->up_scroll));
	gtk_widget_set_parent (self->down_scroll, GTK_WIDGET(self));

	self->scroll = FALSE;

	self->offset = 0;
	self->max_offset = 0;
	self->scroll_by = 0;
	self->scroll_timeout = 0;
}

static void
scroll_menu_destroy (GtkObject *object)
{
	ScrollMenu *self = SCROLL_MENU (object);

	if (self->up_scroll != NULL) {
		/* this should remove the widget and thus cause
		 * a NULL and an unref */
		gtk_widget_destroy (self->up_scroll);
		g_assert (self->up_scroll == NULL);
	}

	if (self->down_scroll != NULL) {
		/* this should remove the widget and thus cause
		 * a NULL and an unref */
		gtk_widget_destroy (self->down_scroll);
		g_assert (self->down_scroll == NULL);
	}

	if (self->scroll_timeout != 0) {
		gtk_timeout_remove (self->scroll_timeout);
		self->scroll_timeout = 0;
	}

	if (GTK_OBJECT_CLASS (parent_class)->destroy)
		GTK_OBJECT_CLASS (parent_class)->destroy (object);
}

static void
scroll_menu_size_request (GtkWidget *widget,
			  GtkRequisition *requisition)
{
	ScrollMenu *self = SCROLL_MENU (widget);
	GtkRequisition child_requisition;
	int screen_height;

	if (GTK_WIDGET_CLASS (parent_class)->size_request)
		GTK_WIDGET_CLASS (parent_class)->size_request (widget,
							       requisition);

	screen_height = gdk_screen_height ();

	if (requisition->height > screen_height) {
		int button_height;

		button_height = 0;
		gtk_widget_size_request (self->up_scroll, &child_requisition);
		button_height += child_requisition.height;
		gtk_widget_size_request (self->down_scroll, &child_requisition);
		button_height += child_requisition.height;

		self->max_offset =
			(requisition->height - screen_height) + button_height;

		if (self->offset > self->max_offset)
			self->offset = self->max_offset;

		requisition->height = screen_height;
		self->scroll = TRUE;
	} else {
		self->offset = 0;
		self->max_offset = 0;
		self->scroll = FALSE;
	}
}

static void
scroll_menu_size_allocate (GtkWidget *widget,
			   GtkAllocation *allocation)
{
	ScrollMenu *self;
	GtkMenu *menu;
	GtkMenuShell *menu_shell;
	GtkWidget *child;
	GtkAllocation child_allocation;
	GtkRequisition child_requisition;
	GList *children;
	int top_scroller_height;

	g_return_if_fail (widget != NULL);
	g_return_if_fail (IS_SCROLL_MENU (widget));
	g_return_if_fail (allocation != NULL);

	self = SCROLL_MENU (widget);
	menu = GTK_MENU (widget);
	menu_shell = GTK_MENU_SHELL (widget);

	widget->allocation = *allocation;
	if (GTK_WIDGET_REALIZED (widget)) {
		gdk_window_move_resize (widget->window,
					allocation->x, allocation->y,
					allocation->width, allocation->height);
	}

	top_scroller_height = 0;

	if (self->scroll) {
		if (GTK_WIDGET_MAPPED (widget) &&
		    ! GTK_WIDGET_MAPPED (self->up_scroll))
			gtk_widget_map (self->up_scroll);
		if (self->up_scroll->window)
			gdk_window_raise (self->up_scroll->window);

		if (GTK_WIDGET_MAPPED (widget) &&
		    ! GTK_WIDGET_MAPPED (self->down_scroll))
			gtk_widget_map (self->down_scroll);
		if (self->down_scroll->window)
			gdk_window_raise (self->down_scroll->window);

		gtk_widget_get_child_requisition (self->up_scroll, &child_requisition);
		child_allocation.x = 0;
		child_allocation.y = 0;
		child_allocation.width = allocation->width;
		child_allocation.height = child_requisition.height;
		gtk_widget_size_allocate (self->up_scroll, &child_allocation);

		top_scroller_height = child_requisition.height;

		gtk_widget_get_child_requisition (self->down_scroll, &child_requisition);
		child_allocation.x = 0;
		child_allocation.y = allocation->height - child_requisition.height;
		child_allocation.width = allocation->width;
		child_allocation.height = child_requisition.height;
		gtk_widget_size_allocate (self->down_scroll, &child_allocation);
	} else {
		if (GTK_WIDGET_MAPPED (widget)) {
			if (GTK_WIDGET_MAPPED (self->up_scroll))
				gtk_widget_unmap (self->up_scroll);
			if (GTK_WIDGET_MAPPED (self->down_scroll))
				gtk_widget_unmap (self->down_scroll);
		}
	}

	if (menu_shell->children) {
		child_allocation.x = (GTK_CONTAINER (menu)->border_width +
				      widget->style->klass->xthickness);
		child_allocation.y = (GTK_CONTAINER (menu)->border_width +
				      widget->style->klass->ythickness) - self->offset + top_scroller_height;
		child_allocation.width = MAX (1, (gint)allocation->width - child_allocation.x * 2);

		children = menu_shell->children;
		while (children) {
			child = children->data;
			children = children->next;

			if (GTK_WIDGET_VISIBLE (child)) {
				gtk_widget_get_child_requisition (child, &child_requisition);

				child_allocation.height = child_requisition.height;

				gtk_widget_size_allocate (child, &child_allocation);
				gtk_widget_queue_draw (child);

				child_allocation.y += child_allocation.height;
			}
		}
	}
}

static void
scroll_menu_map (GtkWidget *widget)
{
	ScrollMenu *self = SCROLL_MENU (widget);

	if (GTK_WIDGET_CLASS (parent_class)->map)
		GTK_WIDGET_CLASS (parent_class)->map (widget);

	if (self->scroll) {
		if ( ! GTK_WIDGET_MAPPED (self->up_scroll))
			gtk_widget_map (self->up_scroll);
		if ( ! GTK_WIDGET_MAPPED (self->down_scroll))
			gtk_widget_map (self->down_scroll);
	}

}

static void
scroll_menu_unmap (GtkWidget *widget)
{
	ScrollMenu *self = SCROLL_MENU (widget);

	if (GTK_WIDGET_CLASS (parent_class)->unmap)
		GTK_WIDGET_CLASS (parent_class)->unmap (widget);

	if (GTK_WIDGET_MAPPED (self->up_scroll))
		gtk_widget_unmap (self->up_scroll);
	if (GTK_WIDGET_MAPPED (self->down_scroll))
		gtk_widget_unmap (self->down_scroll);
}

static void
adjust_item (ScrollMenu *self, GtkWidget *widget)
{
	GtkWidget *menu = GTK_WIDGET (self);
	GtkRequisition requisition;
	int top;
	int bottom;
	int move;

	if ( ! self->scroll)
		return;

	top = (GTK_CONTAINER (self)->border_width +
	       menu->style->klass->ythickness);

	bottom = menu->allocation.height -
		(GTK_CONTAINER (self)->border_width +
		 menu->style->klass->ythickness);

	gtk_widget_get_child_requisition (self->up_scroll, &requisition);
	top += requisition.height;

	gtk_widget_get_child_requisition (self->down_scroll, &requisition);
	bottom -= requisition.height;

	move = 0;

	if (widget->allocation.y < top) {
		move = - (top - widget->allocation.y);
	} else if (widget->allocation.y + widget->allocation.height > bottom) {
		move = (widget->allocation.y +
			widget->allocation.height -
			bottom);
	}

	if (move != 0)
		scroll_by (self, move);
}

static void
scroll_menu_move_current (GtkMenuShell *menu_shell, GtkMenuDirectionType direction)
{
	GtkWidget *current_active = menu_shell->active_menu_item;

	if (GTK_MENU_SHELL_CLASS (parent_class)->move_current)
		GTK_MENU_SHELL_CLASS (parent_class)->move_current (menu_shell, direction);

	if (menu_shell->active_menu_item != NULL &&
	    menu_shell->active_menu_item != current_active)
		adjust_item (SCROLL_MENU (menu_shell), menu_shell->active_menu_item);
}

static void
scroll_menu_remove (GtkContainer *container, GtkWidget *widget)
{
	ScrollMenu *self = SCROLL_MENU (container);

	if (self->up_scroll == widget) {
		gtk_widget_unref (self->up_scroll);
		self->up_scroll = NULL;
	} else if (self->down_scroll == widget) {
		gtk_widget_unref (self->down_scroll);
		self->down_scroll = NULL;
	} else if (GTK_CONTAINER_CLASS (parent_class)->remove) {
		GTK_CONTAINER_CLASS (parent_class)->remove (container, widget);
	}
}

static gboolean
scroll_menu_leave_notify (GtkWidget *widget, GdkEventCrossing *event)
{
	ScrollMenu *self = SCROLL_MENU (widget);
	GtkWidget *event_widget;

	event_widget = gtk_get_event_widget ((GdkEvent*) event);

	if (widget == event_widget ||
	    widget == self->up_scroll ||
	    widget == self->down_scroll) {
		if (self->in_up) {
			scroll_leave_notify (self->up_scroll, event,
					     GINT_TO_POINTER (SCROLL_UP));
		} else if (self->in_down) {
			scroll_leave_notify (self->down_scroll, event,
					     GINT_TO_POINTER (SCROLL_DOWN));
		}
	}

	if (GTK_WIDGET_CLASS (parent_class)->leave_notify_event)
		return GTK_WIDGET_CLASS (parent_class)->leave_notify_event (widget, event);

	return FALSE;
}

static gboolean
scroll_menu_enter_notify (GtkWidget *widget, GdkEventCrossing *event)
{
	ScrollMenu *self = SCROLL_MENU (widget);
	GtkWidget *event_widget;

	event_widget = gtk_get_event_widget ((GdkEvent*) event);

	if (self->up_scroll == event_widget) {
		return scroll_enter_notify (self->up_scroll, event,
					    GINT_TO_POINTER (SCROLL_UP));
	} else if (self->down_scroll == event_widget) {
		return scroll_enter_notify (self->down_scroll, event,
					    GINT_TO_POINTER (SCROLL_DOWN));
	}

	if (self->in_up) {
		GdkWindow *window = gdk_window_at_pointer (NULL, NULL);
		if (window != self->up_scroll->window)
			scroll_leave_notify (self->up_scroll, event,
					     GINT_TO_POINTER (SCROLL_UP));
	} else if (self->in_down) {
		GdkWindow *window = gdk_window_at_pointer (NULL, NULL);
		if (window != self->down_scroll->window)
			scroll_leave_notify (self->down_scroll, event,
					     GINT_TO_POINTER (SCROLL_DOWN));
	}

	
	if (GTK_WIDGET_CLASS (parent_class)->enter_notify_event)
		return GTK_WIDGET_CLASS (parent_class)->enter_notify_event (widget, event);

	return FALSE;
}

GtkWidget *
scroll_menu_new (void)
{
	return (GtkWidget *)gtk_type_new (scroll_menu_get_type ());
}
