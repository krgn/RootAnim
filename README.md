# RootAnim

An evening-spanning venture into some aspects of Haskell I mean to explore for a while, 
particularly the FFI and OpenGL.

FFI code is adapted from
[Graphics.UI.GL](http://hackage.haskell.org/package/GLHUI-1.1.0/docs/Graphics-UI-GLWindow.html),
opening the XDefaultRootWindow instead of creating a new, framed one.

The animation is basically a cheesy little WinAmp swoosh running across the
entire X11 root window.

