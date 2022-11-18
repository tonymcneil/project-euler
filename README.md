# project-euler clojure

Some tinkering on Project Euler Problems in Clojure (babashka variant).

## Running locally

Ensure babashka bb binary is installed on the system path and then run:

    ./00-run-local.sh

Or for test suite run:

    ./00-test-local.sh

## Running from Replit

The hidden .config folder needs to contain the bb bashaka binary,
and the .replit file for global execution looks like this:

    run = ".config/bb --classpath src main.clj"

Or for test suite:

    run = ".config/bb --classpath src:test test.clj"

(Keeping the babashka binary in the hidden folder is a personal preference to keep the workspace clean).