% To-do list for Shady
% Conal Elliott
% Last modified Fri Mar 05, 2010 21:20




-------------

# Simple things

* Comments in MechanicsGL
* Trim constraints.  Eliminate some of the constraints by adding some sub-classing: Pretty < PrettyPrec < HasExpr < HasType.  Use the syntactic subclassing to provide default methods as well as suggested implementations (e.g., Show via Pretty via PrettyPrec via HasExpr).  Other techniques?
* Preserve aspect ratio in viewer
* Move over more Image definitions from Pajama
* More image examples
* Turn off depth & stencil buffers for image synthesis
* Check into pre-multiplied vs post-multiplied alpha.  I'm assuming pre-multiplied.
* Vector space instance for Color
* Study Russell O'Connor's Data.Colour
* Do something fun with numeric and vector-space operations on images
* Add more GLSL primitives.  For instance normalize (in place of current normalized).
* BUG: when I reduce the mesh size in runSurfB', I see zig-zag artifacts.


# Bigger things

* I don't think I'm enabling & disabling vertex attributes correctly.  Currently, my examples all have a single vertex attribute array, so my error is probably masked.  I think the necessary discipline is enable-render-disable.  I now enable a VBO when creating it, and I never disable.
* Adaptive tessellation, based on derivative bounds (derivatives + interval analysis).  For speed, I'd probably need geometry shaders or a unified model (CUDA/OpenCL).
* Interaction.  Re-study [differential manipulation](http://pages.cs.wisc.edu/~gleicher/cgi-bin/list.py?ScriptStuff/papers.html).
* Beyond rectangular domains.  Support odd domains with holes and stitched-together pieces.  I could use a unit square plus a characteristic (boolean) function.  Maybe use with interval analysis or interval constraints.  And maybe an algebra of domain operators.
* Adaptive antialiasing
* Twist, taper, bend.  Twist is super simple: height-varying rotation.
* Improve CSE by hash-consing.  Polymorphic recursion makes it tricky.
* Add image view parameters (uniforms), including zoom & pan
* Add 3D view parameters, including virtual trackball
* Import discrete images.  How to wrap up as shady images?
* Finalizers for shaders and for vertex arrays.  Ivan is working on a library for managing these resources.
* Multi-pass.  How to decompose when things get too complex, e.g., too many simultaneous textures?  Pat Hanrahan's group has [an old publication](http://www-graphics.stanford.edu/papers/rds/).
* Better examples!
* TODO: Extract computations that depend on only uniforms so that they don't get recomputed, e.g., sin & cos for a uniform angle. Will be a bit tricky, since I now have to partition my uniforms into the primitive part and the derived part.  The sink takes only the primitive part, and it computes and sinks the derived part.
* Anti-aliasing.  How to do it?  Maybe as explicit multiple passes with sub-pixel jitter as in [Pajama](http://conal.net/Pajama).
* Fun UI stuff.
* Can we make Op extensible?  Most of the required info is already in OpInfo, so OpInfo could almost be the representation.  Missing: simplifications.  May be difficult, since GADTs & pattern matching work so well together.


# Done

* Get Data.Vec to work under ghci, and move it to Shady.  How can ghci find the include files?  I think ghci should be finding it via the package db, but it isn't.
* AdditiveGroup and VectorSpace instances for Vec{2,3,4}.
* Eliminate Vec1 type, so that scalar types are valid as is.
* Watch out for replication in fromInteger and fromRational
* See if I can abandon Vec1 etc and use tuples
* Rearrange code much more sensibly.
* Synthetic textures on surfaces
* Revisit Vector as a type *constructor* class.  Maybe instead a binary class, indexed by scalar and vector types.  Various code will get simpler, and I'll be able to use "dot" insteadof "dotE" (etc).
* Color module.
* CSE.  What approach to use, especially considering the statically typed syntax representations.
* Animation
* Model swizzling.  Maybe a generalization of GetX etc.  Then rewrite some vector constructions into swizzle form.
* More vectorization
* Detect and insert dot products.
* Eliminate HasPrims?
* Simple save/load.  Not yet separated out from compiler code base.
