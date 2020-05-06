# Elm Test Journey #

`elm-test-journey` aims to achieve most of the benefits associated w/ acceptance tests:
1. Written from the user's perspective
2. Independence from implementation
3. Ensure proper interaction between components

while retaining most of the benefits of unit tests:
1. Fast
2. Type-checked
3. Easy to debug
4. Single threaded

# Example #

See [TodoExample.Effect](examples/tests/TodoExampleTest.elm)


# Effects #

Instead of mocking out low level Cmd's, the application under test defines an `Effect` type to represent all the side-effects your application can have.

This makes test writing much less error prone and easier to maintain by writing our tests in terms of type-checkable `Effects`, instead of expected Http requests made up of evil strings.

See [TodoExample.Effect](examples/src/TodoExample.elm)


# Page Object #

The page object pattern separates the test's knowledge of the HTML's structure from it's tests around the flow of the application.  `elm-test-journey` enables you to easily define one for your application, which provides your tests more compile-time checks and better error messages, while reusing selectors.


See [TodoExamplePage](examples/test/TodoExamplePage.elm)

