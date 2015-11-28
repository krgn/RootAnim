#define GL_GLEXT_PROTOTYPES
#define GLX_GLXEXT_PROTOTYPES

#include <stdio.h>
#include <time.h>
#include <X11/Xlib.h>
#include <GL/gl.h>
#include <GL/glx.h>

int WindowInit(unsigned int const major, unsigned int const minor);
void WindowKill();

double WindowTime();
double WindowDTime();

unsigned WindowFrame();

int WindowPointerX();
int WindowPointerX();

void WindowLoop(void (*)());
