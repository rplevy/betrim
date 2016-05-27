# Betrim

Trimmed-down version of ptaoussanis/timbre

Definition: betrim (verb) to adorn on both or all sides.  The word
"betrim" is an anagram of "timbre".  Because betrim only includes the
essentials and supports only newer versions of Clojure, you don't need
to deal with conflicts arising from needless ancient dependencies.

In leiningen config:

```clojure
[rplevy/betrim "4.4.0"] ; Stable
```

In clj/cljs namespaces:
```clojure

;; clojure
(require [rplevy.betrim :as betrim
          :refer [log trace debug info warn error fatal report logf tracef
                  debugf infof warnf errorf fatalf reportf spy
                  get-env log-env]])

;; clojurescript
(:require [rplevy.betrim :as timbre
           :refer-macros [log trace debug info warn error fatal report logf
                          tracef debugf infof warnf errorf fatalf reportf spy
                          get-env log-env]])
```

## Differences from Timbre

* Does not include any special-purpose appenders.  Because of this, it does not depend on any libraries not directly supporting logging functionality.
* Does not explicitly support older versions of Clojure
* Does not include a profiling tool
* Does not depend on a utilities library.  Removes thousands of lines of Encore code.
* Uses cljc / reader conditionals in place of cljx.
* Uses core memoize in place of hand-rolled memoize.

## License

Distributed under the [EPL v1.0] \(same as Clojure).

Copyright &copy; 2015-2016 Peter Taoussanis. (https://github.com/ptaoussanis/timbre)
Copyright &copy; 2016-2017 Robert P. Levy.