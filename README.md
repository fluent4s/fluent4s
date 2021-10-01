# Fluent4s

Fluent4s is a Scala implementation of [The Project Fluent](https://projectfluent.org/),
a new spec for translation files.

## Features

- Functional error handling
- Error accumulation
- [Fluent Spec](https://github.com/projectfluent/fluent/wiki#background)

## Import in your project

<details>
<summary>SBT</summary>

```sbt
libraryDependencies += "io.github.fluent4s" %% "fluent4s-core" % "version"
libraryDependencies += "io.github.fluent4s" %% "fluent4s-parser" % "version"
```

</details>

<details>
<summary>Mill</summary>

```scala
ivy"io.github.fluent4s::fluent-core:version"
ivy"io.github.fluent4s::fluent-parser:version"
```

</details>


## Usage

### Basic import

The user api of Fluent4s is located inside the `io.github.fluent4s.api` package:
```scala
import io.github.iltotore.fluent4s.api._
```

Also import the parser of your choice. Here is the default parser:
```scala
import io.github.iltotore.fluent4s.parser.Ftl.Parser
```

### Load a Fluent text

You firstly need to use the `decode` method to parse and resolve your fluent text:
```scala
    val ftl =
  """# Simple things are simple.
    |hello-user = Hello, {$userName}!
    |
    |# Complex things are possible.
    |shared-photos =
    |    {$userName} {$photoCount ->
    |        [one] added a new photo
    |       *[other] added {$photoCount} new photos
    |    } to {$userGender ->
    |        [male] his stream
    |        [female] her stream
    |       *[other] their stream
    |    }.""".stripMargin

val resource: FluentResource = decode(ftl, Locale.ENGLISH)
```

### Evaluate a message

FluentResource has two methods to get a message from a key:
```scala
val resource: FluentResource = ???

//Using tuples
resource.getMessage("shared-photos")(
  "userName" -> FluentValue.Text("Il_totore"),
  "photoCount" -> FluentValue.Number(5),
  "userGender" -> "male"
)

//Using a Map
val args = Map(
  "userName" -> FluentValue.Text("Il_totore"),
  "photoCount" -> FluentValue.Number(5),
  "userGender" -> "male"
)

resource.getMessageWith("shared-photos")(args)
```

Find more information about methods and classes in the [Scaladoc](/docs/scaladoc)