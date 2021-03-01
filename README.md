# Bs-Batteries Test Repo

Im trying to use https://github.com/meafmira/bs-batteries but cant seem to get the compiler to see the dependency.

```

# Error

```sh
 We've found a bug for you!
  /Volumes/SSD/Github/bs-batteries-test/src/Demo.re:2:6-14

  1 │ Js.log("Hello, ReScript!");
  2 │ open Batteries;
  3 │ // module Option = Batteries.Option;
  4 │ let odd = (x) =>{

  The module or file Batteries can't be found.
```

This is the test repo https://github.com/idkjs/bs-batteries-test

I have bs-batteries in bsconfig.json. I tried patching bs-batteries bsconfig.json adding both `namespace:true` or `namespace:"Batteries"` and neither worked. Any idea why I cant access the lib? Thanks.
