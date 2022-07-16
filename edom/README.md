
Elm Client
==========

### Build

    > cd edom
    > elm make Main.elm --output build.js

Then go resave JS.hs, or change to non-embed for testing


### Production build

    npm install -g uglify-js
    cd edom
    elm make Main.elm --output build.js --optimize
    uglifyjs build.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle > build.min.js

To build for production add --optimize