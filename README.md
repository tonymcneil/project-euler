# project-euler clojure

Some tinkering on Project Euler Problems in Clojure (babashka variant).

## Local running prerequisites

Ensure babashka bb binary is installed on the system path and then run:

    ./00-run-local.sh
## Replit prerequisites

The hidden .config folder needs to contain the bb bashaka binary,
and the .replit file for global execution looks like this:

    run = ".config/bb --classpath src main.clj"

(Keeping the babashka binary in the hidden folder is a personal preference to keep the workspace clean).