# CL-Arrows

Implements threading macros, inspired by Clojure (both core and
the [swiss-arrows](https://github.com/rplevy/swiss-arrows) library).

This is an ASDF system providing the package `cl-arrows`.

## Overview

You get:

- the basic “thrushing” arrows `->` and `->>`,
- “diamond” arrows `-<>` and `-<>>`,
- binding arrow `as->`,
- “maybe” arrows `some->` and `some->>`,
- conditional arrows `cond->` and `cond->>`, and
- “double arrow cancellers” `->*` and `as->*`.

As far as I see, `->*` and `as->*` are new.  Their purpose is to be nested in
other threading forms to temporarily supplant their behaviour (see “Nesting”
below).

## Other arrow libraries

- `arrow-macros`

## Notable differences to Clojure and swiss-arrows

- `Cond->` and `cond->>` use one additional paren nesting for the clauses, so
  that each clause can contain multiple forms to thread/execute.

- `-<>` and `-<>>` do not support literals to insert the `<>` placeholder.  The
  placeholder really only works at the outermost level of the threaded forms.
  The reason for this is mostly that Common Lisp does not have so many literal
  syntax elements (by default) where it would make sense to do this kind of
  insertion.  If you do need anything fancy, use `as->` or `as->*` for a real
  lexical binding.

## Notable differences to arrow-macros

- `-<>` and `-<>>` do not use a code walker to find out whether a placeholder is
  present in the next threaded form.  This reduces the dependencies of
  `cl-arrows` (there are none at present).  Instead, the recommendation is to
  use binding arrows `as->` or `as->*`, possibly nested (see below).

## Nesting

One useful idiom is to nest these arrows.  The basic example is to use `->>`
inside `->`:

    (-> deeply-nested-plist
        (getf :foo)
        (getf :bar)
        (->> (mapcar #'reverse)))

This inspired the discovery of `->*`, which enables the inverse nesting:

    (->> deeply-nested-alist
         (assoc :foo)
         cdr
         (assoc :bar)
         cdr
         (->* (mod 3))
         (expt 2))

## Documentation

[macro]  
`->` initial-form _&rest_ forms => results

Inserts INITIAL-FORM as first argument into the first of FORMS, the result into
the next, etc., before evaluation.  FORMS are treated as list designators.


[macro]  
`->>` initial-form _&rest_ forms => results

Like `->`, but the forms are inserted as last argument instead of first.

[macro]  
`-<>` initial-form _&rest_ forms => results

Like `->`, but if a form in FORMS has one or more symbols named `<>` as
top-level element, each such symbol is substituted by the primary result of the
form accumulated so far, instead of it being inserted as first argument.  Also
known as diamond wand.

[macro]  
`-<>>` initial-form _&rest_ forms => results

Like `-<>`, but if a form in FORMS has no symbols named `<>` as top-level element,
insertion is done like in `->>`.  Also known as diamond spear.

## Examples

    (-> 3
        /)  ; insert into designated list (/)
    => 1/3

    (-> 3
        (expt 2))  ; insert as first argument
    => 9
    
    (->> 3
         (expt 2))  ; insert as last argument
    => 8

    (-<>> (list 1 2 3)
          (remove-if #'oddp <> :count 1 :from-end t) ; substitute <>
          (reduce #'+)                               ; insert last
          /)                                         ; list designator
    => 1/3

    (let ((x 3))
      (-<> (incf x)     ; (let ((r (incf x)))
           (+ <> <>)))  ;   (+ r r))
    => 8
