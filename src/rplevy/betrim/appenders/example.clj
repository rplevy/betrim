(ns rplevy.betrim.appenders.example
  "You can copy this namespace if you'd like a starting template for
  writing your own Betrim appender. PRs for new appenders welcome!

  TODO Please document any dependency GitHub links here, e.g.:
  Requires https://github.com/clojure/java.jdbc,
           https://github.com/swaldman/c3p0"
  {:author "TODO Your Name (@your-github-username)"}
  (:require
   [rplevy.betrim :as betrim]))

;; TODO If you add any special ns imports above, please remember to update
;; Betrim's `project.clj` to include the necessary dependencies under
;; the `:test` profile

(defn example-appender ; Appender constructor
  "Docstring to explain any special opts to influence appender construction,
  etc. Returns the appender map."

  ;; []
  [& [opts]] ; Only necessary if your appender constructor takes any special opts

  (let [{:keys []} opts] ; Destructure any special appender constructor opts

    ;; We'll return a new appender (just a map),
    ;; see `betrim/example-config` for info on all available keys:

    {:enabled?   true  ; Please enable new appenders by default
     :async?     false ; Use agent for appender dispatch? Useful for slow dispatch
     :min-level  nil   ; nil (no min level), or min logging level keyword

     ;; Provide any default rate limits?
     ;; :rate-limit nil
     :rate-limit [[5   1000] ; 5 calls/min
                  [100 60000] ; 100 calls/hour
                  ]

     :output-fn :inherit ; or a custom (fn [data-map]) -> string

     ;; The actual appender (fn [data-map]) -> possible side effects
     :fn
     (fn [data-map]
       (let [;; See `betrim/example-config` for info on all available keys:
             {:keys [instant level ?err_ vargs_ output-fn
                     config   ; Entire Betrim config map in effect
                     appender ; Entire appender map in effect
                     ]}
             data-map

             ?err  @?err_  ; An error, or nil
             vargs @vargs_ ; Vector of raw logging args

             ;; You'll often want an output string with ns, timestamp, vargs, etc.
             ;; A (fn [data]) -> string formatter is provided under the :output-fn
             ;; key, defined as:
             ;; `(or (:output-fn <this appender's map>)
             ;;      (:output-fn <user's config map)
             ;;      betrim/default-output-fn)`
             ;;
             ;; Users therefore get a standardized way to control appender ouput
             ;; formatting for all participating appenders. See
             ;; `rplevy.betrim/default-output-fn` source for details.
             ;;
             output-str (output-fn data-map)]

         ;; This is where we produce our logging side effects. In this case
         ;; we'll just call `println`:
         (println output-str)))}))

(comment

  ;; Create an example appender with default options:
  (example-appender)

  ;; Create an example appender with default options, but override `:min-level`:
  (merge (example-appender) {:min-level :debug}))
