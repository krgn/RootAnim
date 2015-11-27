#include "RootWindow.h"

static Display *display;
static Window window;
static GLXContext context;
static int width, height, scrwidth, scrheight;
static unsigned frame = 0;
static double ftime, dtime;
static int fullscreen = 0;

typedef struct
{
    unsigned long	flags;
    unsigned long	functions;
    unsigned long	decorations;
    long			    inputMode;
    unsigned long	status;
} Hints;

// OpenGL 3 specific functions:
// GLXContext glXCreateContextAttribsARB (Display *, GLXFBConfig, GLXContext, Bool direct, const int *);
typedef GLXContext (*glXCreateContextAttribsARBProc)(Display*, GLXFBConfig, GLXContext, Bool, const int*);
typedef Bool (*glXMakeContextCurrentARBProc)(Display*, GLXDrawable, GLXDrawable, GLXContext);
static glXCreateContextAttribsARBProc createContext = 0;
static glXMakeContextCurrentARBProc makeContextCurrent = 0;

static const int attrs[] = { GLX_RGBA, GLX_DEPTH_SIZE, 24, GLX_DOUBLEBUFFER, None };

// This function creates a throw away context that is only used to query
// for support of specific features, like what version of opengl is supported

// the minimal context needs a display and a visual;

int WindowInit(unsigned int const major, unsigned int const minor)
{
  display = XOpenDisplay(NULL);
	if(display == NULL) {
		puts("Cannot connect to the X server.\n");
		return -1;
	}

	int nscreen = 0;

	scrwidth = XDisplayWidth(display, nscreen);
	scrheight = XDisplayHeight(display, nscreen);

	width = scrwidth;
	height = scrheight;

	XVisualInfo *visual = glXChooseVisual(display, nscreen, (int*)attrs);
	if(visual == NULL) {
		puts("Could not find a visual.\n");
		return -2;
	}

  window = XDefaultRootWindow(display);
  context = glXCreateContext(display, visual, NULL, GL_TRUE);
	glXMakeCurrent(display, window, context);
	XFree(visual);

	return 1;
}


void WindowLoop(void (*Loop)())
{
	struct timespec ts[3];	// even, odd, initial
	int done = 0;
	XEvent event;

	clock_gettime(CLOCK_MONOTONIC, ts+2);
	ts[0] = ts[1] = ts[2];

	while(!done)
	{
		while(XPending(display) > 0)
		{
			XNextEvent(display, &event);
			switch(event.type)
			{
			case ConfigureNotify:
				if((event.xconfigure.width != width) || (event.xconfigure.height != height))
				{
					width = event.xconfigure.width;
					height = event.xconfigure.height;
					glViewport(0, 0, width, height);
				}
				break;

			case ButtonPress:
				done = 1;
				break;
			}
		}

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
    puts("Bye.");
	if(context) {
        XDestroyWindow(display, window);
		glXMakeCurrent(display, 0, 0);
		glXDestroyContext(display, context);
		context = 0;
	}

	XCloseDisplay(display);
}

double WindowTime(){ return ftime; }
double WindowDTime(){ return dtime; }
