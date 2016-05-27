(ns rplevy.betrim
  "Simple(r), flexible logging for Clojure/Script. No XML."
  {:author "Robert P. Levy"}
  (:require
   #?(:clj [clojure.core.memoize :as memo])
   #?(:clj [clojure.edn :as edn])
   [clojure.string :as str]
   #?(:clj [io.aviso.exception :as aviso-ex])
   [rplevy.betrim.utils :as ut]
   [rplevy.betrim.appenders.core :as core-appenders])
  #?(:cljs
     (:require-macros [rplevy.betrim :as timbre-macros :refer ()])))

;;;; Config

#?(:clj
   (def default-timestamp-opts
    "Controls (:timestamp_ data)"
    {:pattern     "yy-MM-dd HH:mm:ss" #_:iso8601
     :locale      :jvm-default
     :timezone    :utc}))

(declare stacktrace)
(defn default-output-fn
  "Default (fn [data]) -> string output fn.
  You can modify default options with `(partial default-output-fn <opts-map>)`."
  ([data] (default-output-fn nil data))
  ([{:keys [no-stacktrace? stacktrace-fonts] :as opts} data]
   (let [{:keys [level ?err_ vargs_ msg_ ?ns-str hostname_
                 timestamp_ ?line]} data]
     (str
       #?(:clj @timestamp_) #?(:clj " ")
       #?(:clj @hostname_)  #?(:clj " ")
       (str/upper-case (name level))  " "
       "[" (or ?ns-str "?") ":" (or ?line "?") "] - "
       (force msg_)
       (when-not no-stacktrace?
         (when-let [err (force ?err_)]
           (str "\n" (stacktrace err opts))))))))

;;; Alias core appenders here for user convenience
(declare default-err default-out)
#?(:clj  (ut/defalias          core-appenders/println-appender))
#?(:clj  (ut/defalias          core-appenders/spit-appender))
#?(:cljs (def println-appender  core-appenders/println-appender))
#?(:cljs (def console-appender  core-appenders/console-appender))

(def example-config
  "Example (+default) Timbre v4 config map.

  APPENDERS
    An appender is a map with keys:
      :min-level       ; Level keyword, or nil (=> no minimum level)
      :enabled?        ;
      :async?          ; Dispatch using agent? Useful for slow appenders
      :rate-limit      ; [[ncalls-limit window-ms] <...>], or nil
      :output-fn       ; Optional override for inherited (fn [data]) -> string
      :fn              ; (fn [data]) -> side effects, with keys described below

    An appender's fn takes a single data map with keys:
      :config          ; Entire config map (this map, etc.)
      :appender-id     ; Id of appender currently dispatching
      :appender        ; Entire map of appender currently dispatching

      :instant         ; Platform date (java.util.Date or js/Date)
      :level           ; Keyword
      :error-level?    ; Is level e/o #{:error :fatal}?
      :?ns-str         ; String, or nil
      :?file           ; String, or nil
      :?line           ; Integer, or nil ; Waiting on CLJ-865

      :?err_           ; Delay - first-arg platform error, or nil
      :vargs_          ; Delay - raw args vector
      :hostname_       ; Delay - string (clj only)
      :msg_            ; Delay - args string
      :timestamp_      ; Delay - string
      :output-fn       ; (fn [data]) -> formatted output string
                       ; (see `default-output-fn` for details)

      :context         ; *context* value at log time (see `with-context`)

  MIDDLEWARE
    Middleware are simple (fn [data]) -> ?data fns (applied left->right) that
    transform the data map dispatched to appender fns. If any middleware returns
    nil, NO dispatching will occur (i.e. the event will be filtered).

  The `example-config` source code contains further settings and details.
  See also `set-config!`, `merge-config!`, `set-level!`."

  {:level :debug  ; e/o #{:trace :debug :info :warn :error :fatal :report}

   ;; Control log filtering by namespaces/patterns. Useful for turning off
   ;; logging in noisy libraries, etc.:
   :ns-whitelist  [] #_["my-app.foo-ns"]
   :ns-blacklist  [] #_["taoensso.*"]

   :middleware [] ; (fns [data]) -> ?data, applied left->right

   #?(:clj :timestamp-opts)
   #?(:clj default-timestamp-opts)

   :output-fn default-output-fn

   :appenders
   #?(:clj {:println (println-appender {:stream :auto})})

   #?(:cljs {:console (console-appender {})})})

(defonce ^:dynamic *config* example-config)
(defmacro with-config        [config & body] `(binding [*config* ~config] ~@body))
(defmacro with-merged-config [config & body]
  `(binding [*config* (ut/nested-merge *config* ~config)] ~@body))

(defn swap-config! [f & args]
  #?(:cljs (set! *config* (apply f *config* args)))
  #?(:clj  (apply alter-var-root #'*config* f args)))

(defn   set-config! [m] (swap-config! (fn [_old] m)))
(defn merge-config! [m] (swap-config! (fn [old] (ut/nested-merge old m))))

(defn     set-level! [level] (swap-config! (fn [m] (merge m {:level level}))))
(defmacro with-level [level & body]
  `(binding [*config* (merge *config* {:level ~level})] ~@body))

;;;; Levels

(def ordered-levels [:trace :debug :info :warn :error :fatal :report])
(def ^:private scored-levels (zipmap ordered-levels (next (range))))
(def ^:private valid-levels  (set ordered-levels))
(def ^:private valid-level
  (fn [level]
    (or (valid-levels level)
        (throw (ex-info (str "Invalid logging level: " level) {:level level})))))

(defn level>= [x y] (>= (long (scored-levels (valid-level x)))
                        (long (scored-levels (valid-level y)))))

#?(:clj (defn- sys-val [id]
          (when-let [s (or (System/getProperty id)
                           (System/getenv      id))]
            (edn/read s))))

#?(:clj
   (def ^:private compile-time-level
     (let [assertee (keyword (or (sys-val "TIMBRE_LEVEL")
                                 (sys-val "TIMBRE_LOG_LEVEL")))]
       (assert (or (nil? assertee)
                   (valid-level assertee)))))
   :cljs
   (def ^:private compile-time-level nil))

;;;; ns filter

(def ^:private compile-ns-filters
  "(fn [whitelist blacklist]) -> (fn [ns]) -> ?unfiltered-ns"
  (let [->re-pattern
        (fn [x]
          (ut/cond!
            (ut/re-pattern? x) x
            (string? x)
            (let [s (-> (str "^" x "$")
                        (str/replace "." "\\.")
                        (str/replace "*" "(.*)"))]
              (re-pattern s))))]

    (memoize
      (fn [whitelist blacklist]
        (let [whitelist* (mapv ->re-pattern whitelist)
              blacklist* (mapv ->re-pattern blacklist)

              white-filter
              (cond
                ;; (nil? whitelist)  (fn [ns] false) ; Might be surprising
                (empty?  whitelist*) (fn [ns] true)
                :else (fn [ns] (some #(re-find % ns) whitelist*)))

              black-filter
              (cond
                (empty? blacklist*) (fn [ns] true)
                :else (fn [ns] (not (some #(re-find % ns) blacklist*))))]

          (fn [ns] (when (and (white-filter ns) (black-filter ns)) ns)))))))

(def ^:private ns-filter
  "(fn [whitelist blacklist ns]) -> ?unfiltered-ns"
  (memoize
    (fn [whitelist blacklist ns]
      {:pre [(assert (string? ns))]}
      ((compile-ns-filters whitelist blacklist) ns))))

#?(:clj
   (def ^:private compile-time-ns-filter
     (let [whitelist (assert (or (nil? (sys-val "TIMBRE_NS_WHITELIST"))
                                 (vector? (sys-val "TIMBRE_NS_WHITELIST"))))
           blacklist (assert (or (nil? (sys-val "TIMBRE_NS_BLACKLIST"))
                                 (vector? (sys-val "TIMBRE_NS_BLACKLIST"))))]
       (when compile-time-level
         (println (str "Compile-time (elision) Timbre level: " compile-time-level)))
       (when whitelist
         (println (str "Compile-time (elision) Timbre ns whitelist: " whitelist)))
       (when blacklist
         (println (str "Compile-time (elision) Timbre ns blacklist: " blacklist)))

       (fn [ns] (ns-filter whitelist blacklist ns))))
   :cljs
   (def ^:private compile-time-ns-filter nil))

;;;; Utils

(declare get-hostname)

(defn- ->delay [x] (if (delay? x) x (delay x)))

(defn- str-join [xs]
  (ut/str-join " "
               (map
                (fn [x]
                  (let [x (ut/nil->str x)] ; Undefined, nil -> "nil"
                    (cond
                      (record?          x) (pr-str x)
                      :else x))))
               xs))

(defn default-data-hash-fn
  "Used for rate limiters, some appenders (e.g. Carmine), etc.
  Goal: (hash data-1) = (hash data-2) iff data-1 \"the same\" as data-2 for
  rate-limiting purposes, etc."
  [data]
  (let [{:keys [?hash-arg ?ns-str ?line vargs_]} data]
    (str (or ?hash-arg ; An explicit hash given as a0
             [?ns-str (or ?line @vargs_)]))))

#?(:clj
   (defonce ^:private get-agent
     (memoize (fn [appender-id] (agent nil :error-mode :continue)))))

(defonce ^:private get-rate-limiter
  (memoize (fn [appender-id specs] (ut/rate-limiter* specs))))

(defn- inherit-over [k appender config default]
  (or
    (let [a (get appender k)] (when-not (ut/kw-identical? a :inherit) a))
    (get config k)
    default))

(defn- inherit-into [k appender config default]
  (merge default
    (get config k)
    (let [a (get appender k)] (when-not (ut/kw-identical? a :inherit) a))))

;;;; Internal logging core

(def ^:dynamic *context*
  "General-purpose dynamic logging context. Context will be included in appender
  data map at logging time." nil)

(defmacro with-context [context & body] `(binding [*context* ~context] ~@body))

(defn log?
  "Runtime check: would Timbre currently log at the given logging level?
    * `?ns-str` arg required to support ns filtering
    * `config`  arg required to support non-global config"
  ([level               ] (log? level nil     nil))
  ([level ?ns-str       ] (log? level ?ns-str nil))
  ([level ?ns-str config]
   (let [config       (or config *config*)
         active-level (or (:level config) :report)]
     (and
       (level>= level active-level)
       (ns-filter (:ns-whitelist config) (:ns-blacklist config) (or ?ns-str ""))
       true))))

(defn- vargs->margs "Processes vargs to extract special a0s"
  [vargs a0-err?]
  (let [[v0 :as v] vargs
        [?err v]
        (if (and a0-err? (ut/error? v0))
          [v0 (ut/vnext v)]
          [nil v])

        [v0 :as v] v
        [?hash-arg v]
        (if (and (map? v0) (contains? v0 :timbre/hash))
          [(:timbre/hash v0) (ut/vnext v)]
          [nil v])]

    {:?err ?err :?hash-arg ?hash-arg :vargs v}))

(defn -log! "Core low-level log fn. Implementation detail!"
  [config level ?ns-str ?file ?line msg-type ?err vargs_ ?base-data]
  (when (log? level ?ns-str config) ; Runtime check
    (let [instant    (ut/now-dt)
          context    *context*

          a0-err?    (ut/kw-identical? ?err :auto)
          margs_     (delay (vargs->margs @vargs_ a0-err?))
          ?err_      (delay (if a0-err? (:?err      @margs_) ?err))
          ?hash-arg_ (delay             (:?hash-arg @margs_))
          vargs_     (delay             (:vargs     @margs_))

          data
          (merge ?base-data
            ;; No, better nest than merge (appenders may want to pass
            ;; along arb context w/o knowing context keys, etc.):
            (when (map? context) context) ; DEPRECATED, for back compat
            {:config     config ; Entire config!
             :context    context
             :instant    instant
             :level      level
             :?ns-str    ?ns-str
             :?file      ?file
             :?line      ?line
             :?err_      ?err_
             :?hash-arg_ ?hash-arg_
             :vargs_     vargs_
             #?(:clj :hostname_) #?(:clj (delay (get-hostname)))
             :error-level? (#{:error :fatal} level)})

          msg-fn
          (fn [vargs_] ; For use *after* middleware, etc.
            (when-not (nil? msg-type)
              (when-let [vargs (assert (or (nil? @vargs_)
                                           (vector? @vargs_)))]
                (case msg-type
                  :p (str-join vargs)
                  :f (let [[fmt args] (ut/vsplit-first vargs)]
                       (ut/format* fmt args))))))

          ?data
          (reduce ; Apply middleware: data->?data
            (fn [acc mf]
              (let [result (mf acc)]
                (if (nil? result)
                  (reduced nil)
                  result)))
            data
            (:middleware config))

          ;; As a convenience to appenders, make sure that middleware
          ;; hasn't replaced any delays with non-delays
          ?data
          (when-let [data ?data] ; Not filtered by middleware
            (merge data
              {:?err_                 (->delay (:?err_      data))
               :?hash-arg_            (->delay (:?hash-arg_ data))
               :vargs_                (->delay (:vargs_     data))
               #?(:clj :hostname_) #?(:clj (->delay (:hostname_  data)))}))]

      (when-let [data ?data] ; Not filtered by middleware
        (reduce-kv
          (fn [_ id appender]
            (when (and (:enabled? appender)
                       (level>= level (or (:min-level appender) :trace)))

              (let [rate-limit-specs (:rate-limit appender)
                    data-hash-fn (inherit-over :data-hash-fn appender config
                                   default-data-hash-fn)
                    rate-limit-okay?
                    (or (empty? rate-limit-specs)
                        (let [rl-fn     (get-rate-limiter id rate-limit-specs)
                              data-hash (data-hash-fn data)]
                          (not (rl-fn data-hash))))]

                (when rate-limit-okay?
                  (let [{:keys [async?] apfn :fn} appender
                        msg_      (delay (or (msg-fn (:vargs_ data)) #_""))
                        output-fn (inherit-over :output-fn appender config
                                    default-output-fn)

                        #?(:clj timestamp_)
                        #?(:clj
                           (delay
                            (let [timestamp-opts (inherit-into
                                                  :timestamp-opts
                                                  appender config
                                                  default-timestamp-opts)
                                  {:keys [pattern locale timezone]}
                                  timestamp-opts]
                              (.format (ut/simple-date-format
                                        pattern
                                        {:locale locale :timezone timezone})
                                       (:instant data)))))

                        data ; Final data prep before going to appender
                        (merge data
                          {:appender-id  id
                           :appender     appender
                           ;; :appender-opts (:opts appender) ; For convenience
                           :msg_         msg_
                           :msg-fn       msg-fn
                           :output-fn    output-fn
                           :data-hash-fn data-hash-fn
                           #?(:clj :timestamp_) #?(:clj timestamp_)})]

                    (if-not async?
                      (apfn data) ; Allow errors to throw
                      #?(:cljs (apfn data))
                      #?(:clj (send-off
                               (get-agent id)
                               (fn [_] (apfn data))))))))))
          nil
          (:appenders config)))))
  nil)

(defmacro -with-elision
  "Implementation detail.
  Executes body iff given level and ns pass compile-time elision."
  [level-form ns-str-form & body]
  (when (or (nil? compile-time-level)
            (not (valid-levels level-form)) ; Not a compile-time level const
            (level>= level-form compile-time-level))

    (when (or (not (string? ns-str-form)) ; Not a compile-time ns-str const
              (compile-time-ns-filter ns-str-form))
      `(do ~@body))))

(defn- fline [and-form] (:line (meta and-form)))

(defmacro log! ; Public wrapper around `-log!`
  "Core low-level log macro. Useful for tooling, etc.

    * `level`    - must eval to a valid logging level
    * `msg-type` - must eval to e/o #{:p :f nil}
    * `opts`     - ks e/o #{:config :?err :?ns-str :?file :?line
                            :?base-data}

  Supports compile-time elision when compile-time const vals
  provided for `level` and/or `?ns-str`."
  [level msg-type args & [opts]]
  (let [{:keys [?ns-str] :or {?ns-str (str *ns*)}} opts]
    (-with-elision
     level   ; level-form  (may/not be a compile-time kw const)
     ?ns-str ; ns-str-form (may/not be a compile-time str const)
     (let [{:keys [config ?err ?file ?line ?base-data]
            :or   {config 'rplevy.betrim/*config*
                   ?err   :auto ; => Extract as err-type a0
                   ?file  #?(:clj *file*, :cljs nil)
                   ;; NB waiting on CLJ-865:
                   ?line  (fline &form)}} opts

           ?file (when (not= ?file "NO_SOURCE_PATH") ?file)]

       `(-log! ~config ~level ~?ns-str ~?file ~?line ~msg-type ~?err
               (delay [~@args]) ~?base-data)))))

;;;; Main public API-level stuff
;; TODO Have a bunch of cruft here trying to work around CLJ-865 to some extent

;;; Log using print-style args
(defmacro log*  [config level & args] `(log! ~level  :p ~args ~{:?line (fline &form) :config config}))
(defmacro log          [level & args] `(log! ~level  :p ~args ~{:?line (fline &form)}))
(defmacro trace              [& args] `(log! :trace  :p ~args ~{:?line (fline &form)}))
(defmacro debug              [& args] `(log! :debug  :p ~args ~{:?line (fline &form)}))
(defmacro info               [& args] `(log! :info   :p ~args ~{:?line (fline &form)}))
(defmacro warn               [& args] `(log! :warn   :p ~args ~{:?line (fline &form)}))
(defmacro error              [& args] `(log! :error  :p ~args ~{:?line (fline &form)}))
(defmacro fatal              [& args] `(log! :fatal  :p ~args ~{:?line (fline &form)}))
(defmacro report             [& args] `(log! :report :p ~args ~{:?line (fline &form)}))

;;; Log using format-style args
(defmacro logf* [config level & args] `(log! ~level  :f ~args ~{:?line (fline &form) :config config}))
(defmacro logf         [level & args] `(log! ~level  :f ~args ~{:?line (fline &form)}))
(defmacro tracef             [& args] `(log! :trace  :f ~args ~{:?line (fline &form)}))
(defmacro debugf             [& args] `(log! :debug  :f ~args ~{:?line (fline &form)}))
(defmacro infof              [& args] `(log! :info   :f ~args ~{:?line (fline &form)}))
(defmacro warnf              [& args] `(log! :warn   :f ~args ~{:?line (fline &form)}))
(defmacro errorf             [& args] `(log! :error  :f ~args ~{:?line (fline &form)}))
(defmacro fatalf             [& args] `(log! :fatal  :f ~args ~{:?line (fline &form)}))
(defmacro reportf            [& args] `(log! :report :f ~args ~{:?line (fline &form)}))

(defmacro -log-errors [?line & body]
  `(let [[?result# ?error#] (ut/catch-errors ~@body)]
     (when-let [e# ?error#]
       ;; (error e#) ; CLJ-865
       (log! :error :p [e#] ~{:?line ?line}))
     ?result#))

(defmacro -log-and-rethrow-errors [?line & body]
  `(let [[?result# ?error#] (ut/catch-errors ~@body)]
     (when-let [e# ?error#]
       ;; (error e#) ; CLJ-865
       (log! :error :p [e#] ~{:?line ?line})
       (throw e#))
     ?result#))

(defmacro -logged-future [?line & body] `(future (-log-errors ~?line ~@body)))

(defmacro log-errors             [& body] `(-log-errors             ~(fline &form) ~@body))
(defmacro log-and-rethrow-errors [& body] `(-log-and-rethrow-errors ~(fline &form) ~@body))
(defmacro logged-future          [& body] `(-logged-future          ~(fline &form) ~@body))

#?(:clj
   (defn handle-uncaught-jvm-exceptions!
    "Sets JVM-global DefaultUncaughtExceptionHandler."
    [& [handler]]
    (let [handler
          (or handler
              (fn [throwable ^Thread thread]
                (errorf throwable "Uncaught exception on thread: %s"
                        (.getName thread))))]
      (Thread/setDefaultUncaughtExceptionHandler
       (reify Thread$UncaughtExceptionHandler
         (uncaughtException [this thread throwable]
           (handler throwable thread)))))))

(defmacro -spy [?line config level name expr]
  `(-log-and-rethrow-errors ~?line
     (let [result# ~expr]
       ;; Subject to elision:
       ;; (log* ~config ~level ~name "=>" result#) ; CLJ-865
       (log! ~level :p [~name "=>" result#] ~{:?line ?line :config config})

       ;; NOT subject to elision:
       result#)))

(defmacro spy
  "Evaluates named expression and logs its result. Always returns the result.
  Defaults to :debug logging level and unevaluated expression as name."
  ([                  expr] `(-spy ~(fline &form) *config* :debug '~expr ~expr))
  ([       level      expr] `(-spy ~(fline &form) *config* ~level '~expr ~expr))
  ([       level name expr] `(-spy ~(fline &form) *config* ~level  ~name ~expr))
  ([config level name expr] `(-spy ~(fline &form) ~config  ~level  ~name ~expr)))

(defmacro get-env [] `(ut/get-env))

;;;; Misc public utils

#?(:clj
   (defn color-str [color & xs]
     (let [ansi-color
           #(format "\u001b[%sm"
                    (case % :reset  "0"  :black  "30" :red   "31"
                          :green  "32" :yellow "33" :blue  "34"
                          :purple "35" :cyan   "36" :white "37"
                          "0"))]
       (str (ansi-color color) (apply str xs) (ansi-color :reset)))))

#?(:clj (def default-out (java.io.OutputStreamWriter. System/out)))
#?(:clj (def default-err (java.io.PrintWriter.        System/err)))
(defmacro with-default-outs [& body]
  `(binding [*out* default-out, *err* default-err] ~@body))

#?(:clj
   (def get-hostname
     (memo/ttl
      (fn []
        ;; Android doesn't like this on the main thread. Would use a `future` but
        ;; that starts the Clojure agent threadpool which can slow application
        ;; shutdown w/o a `(shutdown-agents)` call
        (let [executor (java.util.concurrent.Executors/newSingleThreadExecutor)
              ^java.util.concurrent.Callable f
              (fn []
                (try
                  (.. java.net.InetAddress getLocalHost getHostName)
                  (catch java.net.UnknownHostException _ "UnknownHost")
                  (finally (.shutdown executor))))]
          (deref (.submit executor f) 5000 "UnknownHost")))
      :ttl/threshold 60000)))

(defn stacktrace [err & [{:keys [stacktrace-fonts] :as opts}]]
  #?(:cljs (str err)) ; TODO Alternatives?
  #?(:clj (let [stacktrace-fonts (if (and (nil? stacktrace-fonts)
                                          (contains? opts :stacktrace-fonts))
                                   {} stacktrace-fonts)]
            (if-let [fonts stacktrace-fonts]
              (binding [aviso-ex/*fonts* fonts] (aviso-ex/format-exception err))
              (aviso-ex/format-exception err)))))

(defmacro sometimes "Handy for sampled logging, etc."
  [probability & body]
   `(do (assert (<= 0 ~probability 1) "Probability: 0 <= p <= 1")
        (when (< (rand) ~probability) ~@body)))
