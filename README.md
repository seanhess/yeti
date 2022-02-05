# juniper


Build Elm
--------

   > cd edom
   > elm make Main.elm --output build.js

Then go resave JS.hs, or change to non-embed for testing

To build for production add --optimize

Todo
-----

- [x] don't send full url. Have the client construct it.
- [ ] begin layout library. Custom classes? Experiment. Does multiple class_ work? Or do you need to go deeper

Long Term
- [ ] Pre-render first page load, instructions. Automatic?
