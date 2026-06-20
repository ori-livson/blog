So, hopefully this post gives you a good starting point for how to serve HTMX entirely in the browser. One can of course skip the `.wasm` integration and use [htmx-serverless](https://github.com/ernestmarcinko/htmx-serverless) with all-JavaScript handlers. Perhaps there are JavaScript implementations for functionally generating JavaScript that are good enough; I'm just a big fan of Haskell and its implementation [Lucid](https://hackage.haskell.org/package/lucid2) --- see [this post](/posts/lucid-htmx-servant-combo/) for more background on why.

If you do go the `.wasm` API route, I'm sure you could come up with better interfaces between the `wasm-dispatcher.js` and `.wasm` API than I did here. But do checkout the code for this project in the [repo](https://github.com/ori-livson/haskell-htmx-to-wasm/); a lot of the tough work (for me) ended up centring around:

- Compiling Haskell to `.wasm` such that the endpoints were visible, and callable from the `.js` (this was my first time using WebAssembly).
- Being able to seamlessly switch to the usual `cabal run` and `cabal repl` commands. This was important because I'd use `cabal run` to generate the initial homepage, and `cabal repl` for debugging.
- Being able to pass `stdout` and `stderr` through to JavaScript's `console.log`. Without this, troubleshooting problems with the `.wasm` is very difficult.

So hopefully my solutions can save you some trouble there too.

Happy coding!