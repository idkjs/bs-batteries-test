let odd = (x) =>
  switch (x mod 2) {
  | 0 => Some(x)
  | _ => None
  };

let t10 =
  odd(10)
  |> Option.map((+)(10))
  |> Option.map((/)(100))
  |> Option.map_default(string_of_int, "No value");

Js.log2("t10 value: ", t10);

let t11 =
  odd(11)
  |> Option.map((+)(10))
  |> Option.map((/)(2))
  |> Option.map_default(string_of_int, "No value");

Js.log2("t11 value: ", t11);

let res = Js.Result.Ok("Hello");

exception Super;

let m =
  switch res {
  | Ok(str) => str
  | Error(_) => raise(Super)
  };

type t0(_);

type typ(_) =
  | Int: typ(int)
  | String: typ(string);

let m: int = [%bs.raw "5"];

let to_string = (type t, t: typ(t), x: t) : string =>
  switch t {
  | Int => string_of_int(x)
  | String => x
  };

Js.log(to_string(String, "Hello"));
