![Build](https://github.com/thematthopkins/elm-test-journey/workflows/Build/badge.svg)

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


## How to install ##
```elm-test install thematthopkins/elm-test-journey```

## Example ##

See [TodoExampleTest](https://github.com/thematthopkins/elm-test-journey/blob/master/examples/tests/TodoExampleTest.elm)


## Effects ##

Instead of mocking out low level `Cmd`'s, the application under test defines an `Effect` type to represent all the side-effects your application can have.

This makes test writing much less error prone and easier to maintain by writing our tests in terms of type-checkable `Effects`, instead of expected Http requests or Ports made up of evil json and strings.

See [TodoExample.Effect](https://github.com/thematthopkins/elm-test-journey/blob/master/examples/src/TodoExample.elm)


## Page Object ##

The page object pattern separates the test's knowledge of the HTML's structure from it's tests around the flow of the application.  `elm-test-journey` enables you to easily define one for your application, which provides your tests more compile-time checks and better error messages, while reusing selectors.


See [TodoExamplePage](https://github.com/thematthopkins/elm-test-journey/blob/master/examples/tests/TodoExamplePage.elm)

## Testing Ports ##
`elm-test-journey` expects you to wrap your Ports in Effects.  Unlike HTTP request though, ports are fire-and-forget and don't generate Msgs.  When running `TestJourney.handleEffect` to handle Ports, you'll use `TestJourney.EffectSeen` instead of `TestJourney.EffectProcessed` to avoid dealing w/ `Msg`s.

## Testing Subscriptions ##

`elm-test-journey` addresses these by using `TestJourney.injectMsg`.  This allows you to simulate the `Msg` that your application would have created from the subscription.

This is where `elm-test-journey`'s approach differs from [elm-test-program](https://github.com/avh4/elm-program-test).  If you'd like to incorporate tests for these lower-level events more directly into your acceptance tests, [elm-test-program](https://github.com/avh4/elm-program-test) may be a better option.
