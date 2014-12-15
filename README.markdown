*Shady* is a high-performance implementation of functional graphics, in the heritage of [Pan](http://conal.net/Pan) and [Vertigo](http://conal.net/Pan).
The components are as follows:

* Compile functional specifications into GPU code: [shady-gen](http://github.com/conal/shady-gen)
* Add graphics: [shady-graphics](http://github.com/conal/shady-graphics) 
* Rendering: [shady-render](http://github.com/conal/shady-render)
* GUIs: [shady-tv](http://github.com/conal/shady-tv)
* Examples: [shady-examples](http://github.com/conal/shady-examples)

Shady has been sitting on the shelf for a few years while I've been waiting for a cross-platform, GHCi- and OpenGL-friendly low-level GUI library in the Haskell ecosystem.

The rendering part appears to be broken currently.
Last time I tried, I got a segfault, presumably in an OpenGL call.
I guess something changed in the OpenGL bindings.

I'm much stronger with language, library, and compiler design than with complex, low-level libraries like OpenGL.
I would *love* help getting things working again.
In particular, I want to overhaul the OpenGL part (in shady-render).
This part was written long ago when Haskell OpenGL bindings lacked several key abilities, so I used [glew](https://github.com/conal/glew) bindings by Ivan Tomac.
