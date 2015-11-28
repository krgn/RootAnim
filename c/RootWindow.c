#include "RootWindow.h"

static Display *display;
static Window window;
static GLXContext context;
static int width, height;
static unsigned frame = 0;
static double ftime, dtime;

static Window window_returned;
static int root_x, root_y;
static int win_x, win_y;
static unsigned int mask_return;

typedef GLXContext
  (*glXCreateContextAttribsARBProc)
  (Display*, GLXFBConfig, GLXContext, Bool, const int*);

typedef Bool
  (*glXMakeContextCurrentARBProc)
  (Display*, GLXDrawable, GLXDrawable, GLXContext);

static const int attrs[] =
  { GLX_RGBA
  , GLX_DEPTH_SIZE, 24
  , GLX_DOUBLEBUFFER
  , None };

int WindowInit(unsigned int const major, unsigned int const minor)
{
  display = XOpenDisplay(NULL);

	if(display == NULL) {
		puts("Cannot connect to the X server.\n");
		return -1;
	}

	width = XDisplayWidth(display, 0);
	height = XDisplayHeight(display, 0);

	XVisualInfo *visual = glXChooseVisual(display, 0, (int*)attrs);
	if(visual == NULL) {
		puts("Could not find a visual.\n");
		return -2;
	}

  window = XDefaultRootWindow(display);
  context = glXCreateContext(display, visual, NULL, GL_TRUE);
	glXMakeCurrent(display, window, context);
	XFree(visual);

  XQueryPointer(display, window, &window_returned,
       &window_returned, &root_x, &root_y, &win_x, &win_y,
       &mask_return);

	return 1;
}

void WindowLoop(void (*Loop)())
{
	struct timespec ts[3];
	int done = 0;
	XEvent event;

	clock_gettime(CLOCK_MONOTONIC, ts+2);
	ts[0] = ts[1] = ts[2];


	while(!done)
	{
    XQueryPointer(display, window, &window_returned,
         &window_returned, &root_x, &root_y, &win_x, &win_y,
         &mask_return);


    glEnable (GL_BLEND);
    glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

		Loop();
		glXSwapBuffers(display, window);
		frame++;
		clock_gettime(CLOCK_MONOTONIC, ts+(frame%2));
		dtime = 1e-9*(ts[frame%2].tv_nsec - ts[!(frame%2)].tv_nsec) + (ts[frame%2].tv_sec - ts[!(frame%2)].tv_sec);
		ftime = 1e-9*(ts[frame%2].tv_nsec - ts[2].tv_nsec) + (ts[frame%2].tv_sec - ts[2].tv_sec);
	}

	//WindowKill(0);
}


void WindowKill()
{
	if(context) {
    XDestroyWindow(display, window);
		glXMakeCurrent(display, 0, 0);
		glXDestroyContext(display, context);
		context = 0;
	}

	XCloseDisplay(display);
}

double WindowTime()  { return ftime;  }
double WindowDTime() { return dtime;  }

unsigned WindowFrame() { return frame;  }

int WindowPointerX() { return root_x; }
int WindowPointerY() { return root_y; }
