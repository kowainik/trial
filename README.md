# trial

[![GitHub CI](https://github.com/kowainik/trial/workflows/CI/badge.svg)](https://github.com/kowainik/trial/actions)
[![Hackage](https://img.shields.io/hackage/v/trial.svg?logo=haskell)](https://hackage.haskell.org/package/trial)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](LICENSE)

The `Trial` Data Structure is a `Either`-like structure that keeps events history
inside. The data type allows to keep track of the `Fatality` level of each such
event entry (`Warning` or `Error`).

## Project Structure

This is a multi-package project that has the following packages inside:

| Package                      | Description                                                                                                                                                  |
|------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `trial`                      | The main package that contains the `Trial` data structure, instances and useful functions to work with the structure. |
| `trial-optparse-applicative` | `Trial` structure integration with the [`optparse-applicative`](https://hackage.haskell.org/package/optparse-applicative) library for Command Line Interface. |
| `trial-tomland`              | `Trial` structure integration with the [`tomland`](https://hackage.haskell.org/package/tomland) library for TOML configurations.   |
| `trial-example`              | Example project with the usage example of the `Trial` data structure. |

## How to use `trial`

`trial` is compatible with the latest GHC versions starting from `8.6.5`.

In order to start using `trial` in your project, you will need to set it up with
the three easy steps:

1. Add the dependency on `trial` in your project's `.cabal` file. For this, you
   should modify the `build-depends` section by adding the name of this library.
   After the adjustment, this section could look like this:

   ```haskell
   build-depends: base ^>= 4.14
                , trial ^>= 0.0
   ```

2. In the module where you plan to use `Trial`, you should add the import:

   ```haskell
   import Trial (Trial (..), fiasco, prettyPrintTrial)
   ```

3. Now you can use the types and functions from the library:

   ```haskell
   main :: IO ()
   main = putStrLn $ prettyPrintTrial $ fiasco "This is fiasco, bro!"
   ```

## Trial Data Structure

Let's have a closer look at the `Trial` data structure.
`Trial` is a sum type that has two constructors:

  - `Fiasco` — represents the unsuccessful state similar to the `Left`
    constructor of `Either`. However, unlike `Left`, `Fiasco` holds a list of all
    `error`-like items that happened along the way. Each such item has a notion of
    `Fatality` (the severity of the error). The following cases cover
    `Fatality`:
      + `Error` — fatal error that led to the final fatal `Fiasco`.
      + `Warning` — non-essential error, which didn't affect the result.
  - `Result` — represents the successful state similar to the `Right`
    constructor of `Either`. However, unlike `Right`, `Result` keeps the list of
    all `error`-like items that happened along the way. All error items are
    warnings as the final result was found anyway.

Schematically, `Trial` has the following internal representation:

```haskell
data Trial e a
           │ │
           │ ╰╴Resulting type
           │
           ╰╴An error item type

    -- | Unsuccessful case
    = Fiasco (DList (Fatality, e))
              │      │         │
              │      │         ╰╴One error item
              │      │
              │      ╰╴Level of damage
              │
              ╰╴Efficient list-container for error type items

    -- | Successful case
    | Result (DList e) a
              │     │  │
              │     │  ╰╴Result
              │     │
              │     ╰╴One warning item
              │
              ╰╴Efficient list-container for warning type items
```

### `Trial` instances

In order to follow the basis idea of the data type, `Trial` uses smart
constructors and different instances to make the structure work the way it
works.

Here are the main points:

* All `Fiasco`s can be created only with the `Error` `Fatality` level.
* The `Fatality` level can be eased only through the `Semigroup` appends of
  different `Trial`s.
* All error items in `Result` should have only `Warning` `Fatality` level. This
  is guaranteed by the `Trial` `Semigroup` and `Applicative` instances.
* `Semigroup` is responsible for the correct collection of history events, their
  `Fatality` level and the final result decision.
* `Semigroup` chooses the latest 'Result' and combines all events.
* Think of `Semigroup` instance as of high-level combinator of your result.
* `Applicative` is responsible for the correct combination of `Trial`s.
* `Applicative` returns `Fiasco`, if at least one value if `Fiasco`, combine all
  events.
* Think of `Applicative` instance as of low-level combinator of your result on the
  record fields level.
* `Alternative` instance could help when the results are not combined but chosen
  instead.
* `Alternative` returns the first `Result`, combines events only inside
  `Fiasco`s.

## Tagged `Trial`

Additionally, there is a `Trial`-like data type that has a notion of the `tag`
inside.

The main difference from `Trial` is that the resulting type contains additional
information of the tag (or source it came from). The type looks like this:

```haskell
type TaggedTrial tag a = Trial tag (tag, a)
```

Due to the described instances implementation, the tag will always be aligned
with the final source it came from.

The library provides different ways to add the tag:
  * Manual with the `withTag` function
  * Using `OverloadedLabels`and the provided `IsLabel` instance for
    `TaggedTrial`.

You can choose the one that is more suitable for your use-case.

## Usage Examples

One of the use cases when one could consider using `Trial` is the configurations
in the application.

If you need to collect configurations from different places, combine the results
into a single configuration, you can find the `Trial` data structure quite
handy. With `trial` you can get the event history for free and also you can keep
track of where the final result for each component of your configurations type
comes from (by using `tag` functionality).

The complete example in the `trial-example` package. It combines CLI, TOML
configuration and the default options provided in the source code.

To run it you can use the following command:

```shell
$ cabal run trial-example
```

For the successful result you can use the CLI and provide necessary information
in order to have the complete configurations:

```shell
$ cabal run trial-example -- --host="abc"
```
