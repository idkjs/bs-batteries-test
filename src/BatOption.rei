/*
 * Options - functions for the option type
 * Copyright (C) 2003 Nicolas Cannasse
 *               2008 David Teller (Contributor)
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
/** Functions for the option type.

    Options are an Ocaml standard type that can be either [None] (undefined)
    or [Some x] where x can be any value. Options are widely used in Ocaml
    to represent undefined values (a little like NULL in C, but in a type
    and memory safe way). This module adds some functions for working with
    options.

    @author Nicolas Cannasse
    @author David Teller
*/;

type t('a) = option('a);

/** [some x] returns [Some x].

    @since 2.2.0
 */
let some: 'a => option('a);

/** [may f (Some x)] calls [f x] and [may f None] does nothing. */
let may: ('a => unit, option('a)) => unit;

/** [map f (Some x)] returns [Some (f x)] and [map f None] returns [None]. */
let map: ('a => 'b, option('a)) => option('b);

/** [bind (Some x) f] returns [f x] and [bind None f] returns [None].

    @example "Our functions return option types. Compose them to propagate [None]."
    {[
      let pick_long case =
        try
          Some (List.find (fun data -> List.length data > 1000) case)
        with Not_found -> None
      let last_null data = List.rindex_of 0 data
      let interesting_positions dataset =
        List.filter_map
          (fun case -> Option.bind last_null (pick_long case))
          dataset
    ]}
*/
let bind: (option('a), 'a => option('b)) => option('b);

/** [apply None x] returns [x] and [apply (Some f) x] returns [f x] */
let apply: (option('a => 'a), 'a) => 'a;

/** [filter f None] returns [None], [filter f (Some x)] returns [Some x]
    if [f x] is true, and [None] otherwise. */
let filter: ('a => bool, option('a)) => option('a);

/** [default x (Some v)] returns [v] and [default x None] returns [x]. */
let default: ('a, option('a)) => 'a;

/** Like {!default}, with the arguments reversed.
    [None |? 10] returns [10], while [Some "foo" |? "bar"] returns ["foo"].

    {b Note} This operator does not short circuit like [( || )] and [( && )].
    Both arguments will be evaluated.

    @since 2.0 */
let (|?): (option('a), 'a) => 'a;

/** Like {!default}, but the default value is passed as a thunk that
    is only computed if needed.

    @since 2.1 */
let default_delayed: (unit => 'a, option('a)) => 'a;

/** [map_default f x (Some v)] returns [f v] and [map_default f x None]
    returns [x]. */
let map_default: ('a => 'b, 'b, option('a)) => 'b;

/** Like {!map_default}, but the default value is passed as a thunk that
    is only computed if needed.

    @since 2.1 */
let map_default_delayed: ('a => 'b, unit => 'b, option('a)) => 'b;

/** [is_none None] returns [true] otherwise it returns [false]. */
let is_none: option('a) => bool;

/** [is_some (Some x)] returns [true] otherwise it returns [false]. */
let is_some: option('a) => bool;

/** [get (Some x)] returns [x].
    @raise Invalid_argument on [get None]. */
let get: option('a) => 'a;

/** [get_exn (Some x) e] returns [x] and [get_exn None e] raises [e]. */
let get_exn: (option('a), exn) => 'a;

/** Compare two options, possibly using custom comparators for the
    value.  [None] is always assumed to be less than [Some _].  The
    parameter [cmp] defaults to [Pervasives.compare]. */
let compare: (~cmp: ('a, 'a) => int=?, option('a), option('a)) => int;

/** Test for equality between option types, possibly using a custom
    equality predicate.  The parameter [eq] defaults to
    [Pervasives.(=)].

    @since 1.4.0
*/
let eq: (~eq: ('a, 'a) => bool=?, option('a), option('a)) => bool;

/** [enum (Some x)] returns the singleton [x], while [enum None] returns
    the empty enumeration. */
let enum: option('a) => BatEnum.t('a);

/** [of_enum e] consumes the first element of [e], if it exists, and
    returns [Some e]. If [e] is empty, return [None]. */
let of_enum: BatEnum.t('a) => option('a);

/** {6 The Option Monad} */;

/**
    This module provides everything needed to write and execute computations
    in the Option monad.
*/
module Monad: {
  /** The type of values in this monad : option */
  type m('a) = option('a);
  /** [return x] puts a value in the Option monad, that is, returns [Some x]. */
  let return: 'a => m('a);
  /** [bind m f] combines the calculation result [m] with the function [f].
        E.g, in the Option monad :
        [bind (Some 1) (fun x -> if x = 1 then Some 4 else None)] returns Some 4. */
  let bind: (m('a), 'a => m('b)) => m('b);
};

/** {6 Boilerplate code}*/;

/*open BatOrd*/
/*val ord : 'a ord -> 'a option ord*/
/** Comparison between optional values
    @since 2.2.0 */;

/** {7 Printing}*/;

/*val print : ('a BatInnerIO.output -> 'b -> unit) -> 'a BatInnerIO.output -> 'b t -> unit*/
/** Operations on options, with labels.*/
module Labels: {
  let may: (~f: 'a => unit, option('a)) => unit;
  let map: (~f: 'a => 'b, option('a)) => option('b);
  let map_default: (~f: 'a => 'b, 'b, option('a)) => 'b;
};

module Infix: {
  /** Like {!default}, with the arguments reversed.
        [None |? 10] returns [10], while [Some "foo" |? "bar"] returns ["foo"]. */
  let (|?): (option('a), 'a) => 'a;
  /** as [Monad.bind] */
  let (>>=): (option('a), 'a => option('b)) => option('b);
};
