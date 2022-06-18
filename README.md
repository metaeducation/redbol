# Redbol

Redbol is an experimental emulation of the semantics of the languages Rebol2
and Red:

* Rebol: http://rebol.com
* Red: http://red-lang.org

The emulation is performed using Ren-C, which is a highly-evolved derivative
of the open-source Rebol3-Alpha codebase:

  https://github.com/metaeducation/ren-c/

At the time of writing, there is no GUI component to the emulation.  However,
the code should run in the WebAssembly browser console for Ren-C...and this
would open doors to those who wanted to implement the GUI dialect on top
of something like Vue.js (or using a Wasm GUI toolkit).
