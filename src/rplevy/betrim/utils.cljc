(ns rplevy.betrim.utils
  #?(:clj (:refer-clojure :exclude (format)))
  #?(:clj (:import  [java.util Date Locale TimeZone]
                    [java.text SimpleDateFormat]))
  #?(:clj (:require [clojure.core.memoize :as memo])
     :cljs (:require [cljs.reader]
                     [cljs.test :as test :refer-macros (is use-fixtures)]
                     [goog.string :as gstr]
                     [goog.string.format]
                     [goog.string.StringBuffer]
                     [goog.net.EventType]
                     [goog.net.ErrorCode])))

;; Recall: no `korks` support due to inherent ambiguous nil ([] vs [nil])
(defn update-in*
  "Like `update-in` but faster, more flexible, and simpler (less ambiguous)"
  [m ks f]
  (if (empty? ks)
    (f m) ; Resolve [] = [nil] ambiguity in `update-in`, `assoc-in`, etc.
    (let [[k & ks] ks]
      (if ks ; Avoid apply calls:
        (assoc m k (update-in* (get m k) ks f))
        (assoc m k (f          (get m k)))))))

(defn dissoc-in
  ([m ks dissoc-k] (update-in* m ks
                               (fn [m] (dissoc m dissoc-k))))
  ([m ks dissoc-k & more] (update-in* m ks
                                      (fn [m] (apply dissoc m dissoc-k more)))))

;;;; Swap stuff

(defn- -platform-cas! "Minor optimization for single-threaded Cljs"
  [atom_ old-val new-val]
  #?(:cljs (do (reset! atom_ new-val) true) ; No compare for our uses
     :clj (.compareAndSet ^clojure.lang.Atom atom_ old-val new-val)))

;; Fastest possible simple swap with [old new] return
(defn dswap! "Returns [<old-val> <new-val>]" [atom_ f]
  #?(:cljs (let [ov @atom_ nv (f ov)] (reset! atom_ nv) [ov nv])
     :clj (loop []
            (let [old-val @atom_
                  new-val (f old-val)]
              (if (.compareAndSet ^clojure.lang.Atom atom_ old-val new-val)
                [old-val new-val]
                (recur))))))

(declare map-keys kw-identical?)

(defrecord Swapped [new-val return-val])
(defn  swapped? [x] (instance? Swapped x))
(defn  swapped  [new-val return-val] (Swapped. new-val return-val))
(defn -swapped "Returns [<new-val> <return-val>]"
  ([x] (if (swapped? x) [(:new-val x) (:return-val x)] [x x]))
  ([old-val    f] (-swapped (f old-val)))
  ([old-val ks f]
   (let [[k1 & kn] ks ; Singular k1 is common case
         m old-val]
     (if kn ; >1 ks
       (if (kw-identical? f :swap/dissoc)
         (-swapped (dissoc-in m (butlast ks) (last ks)))
         (let [old-val-in (get-in m ks)
               [new-val-in return-val] (-swapped (f old-val-in))
               new-val (if (kw-identical? new-val-in :swap/dissoc)
                         (dissoc-in m (butlast ks) (last ks))
                         (assoc-in  m ks new-val-in))]
           [new-val return-val]))

       ;; 0 or 1 ks
       (if (and (nil? k1) (empty? ks))
         (-swapped (f old-val))
         (if (kw-identical? f :swap/dissoc)
           (-swapped (dissoc m k1))
           (let [old-val-in (get m k1)
                 [new-val-in return-val] (-swapped (f old-val-in))
                 new-val (if (kw-identical? new-val-in :swap/dissoc)
                           (dissoc m k1)
                           (assoc  m k1 new-val-in))]
             [new-val return-val])))))))

(defn- -replace-in
  "Reduces input with
  [<type> <ks> <reset-val-or-swap-fn>] or
         [<ks> <reset-val-or-swap-fn>] ops"
  [?vf-type m ops]
  (reduce
    (fn [acc ?op]
      (if-not
       ?op ; Allow conditional ops: (when <pred> <op>), etc.
       acc
       (let [[vf-type ks valf] (if-not ?vf-type ?op (cons ?vf-type ?op))]
         (case vf-type
           :reset (if (empty? ks) valf (assoc-in acc ks valf))
           :swap  (if (nil? valf)
                    acc ; Noop, allows conditional ops
                    (if (empty? ks)
                      (valf acc)
                      ;; Currently ignore possible <return-val>:
                      (nth (-swapped acc ks valf) 0)))))))
    m ops))

(defn replace-in "For use with `swap!`, etc." [m & ops] (-replace-in nil m ops))

(defn- pairs-into [to from] (into to (partition 2  from)))

(defn reset-in! "Is to `reset!` as `swap-in!` is to `swap!`"
  ([atom_ ks new-val]
   (if (empty? ks)
     (reset! atom_ new-val)
     ;; Actually need swap! (CAS) to preserve other keys:
     (swap!  atom_ (fn [old-val] (assoc-in old-val ks new-val)))))

  ([atom_ ks new-val & more] {:pre [(assert (even? (count more)))]}
   (let [op-pairs (pairs-into [[ks new-val]] more)]
     (loop []
       (let [old-val @atom_
             new-val (-replace-in :reset old-val op-pairs)]
         (if (-platform-cas! atom_ old-val new-val)
           [old-val new-val]
           (recur)))))))

#?(:cljs (defn undefined->nil [x] (if (undefined? x) nil x)))

(defn nil->str [x]
  #?(:clj  (if (nil? x) "nil" x)
     :cljs (if (or (undefined? x) (nil? x)) "nil" x)))

(defn format*
  #?(:clj ^String [fmt args]
     :cljs        [fmt args])
  (let [fmt  (or fmt "") ; Prevent NPE
        args (mapv nil->str args)]
    #?(:clj  (String/format fmt (to-array args))
    ;; Removed from cljs.core 0.0-1885, Ref. http://goo.gl/su7Xkj (pulls in a
    ;; lot of Google Closure that's not v. friendly to dead code elimination):
       :cljs (apply gstr/format fmt args))))

(defn error? [x] (instance? #?(:cljs js/Error
                               :clj Throwable)
                            x))

(defmacro cond! "Like `cond` but throws on no-match like `case` and `condp`"
  [& clauses] `(cond ~@clauses :else (throw (ex-info "No matching clause" {}))))

(defn name-with-attrs
    "Given a name symbol and sigs, returns [<name-with-attrs-meta> <args>]"
    ([sym             sigs] (name-with-attrs sym nil sigs))
    ([sym attrs-merge sigs]
     (let [[?docstring sigs] (if (string? (first sigs)) [(first sigs) (next sigs)] [nil sigs])
           [attrs      sigs] (if (map?    (first sigs)) [(first sigs) (next sigs)] [{}  sigs])
           attrs (if ?docstring (assoc attrs :doc ?docstring) attrs)
           attrs (if (meta sym) (conj (meta sym) attrs) attrs)
           attrs (conj attrs attrs-merge)]
       [(with-meta sym attrs) sigs])))

(defmacro declare-remote
  "Declares the given ns-qualified name symbols, preserving symbol metadata.
  Useful for circular dependencies."
  [& syms]
  (let [original-ns (str *ns*)]
    `(do ~@(map (fn [s]
                  (let [ns (namespace s)
                        v  (name      s)
                        m  (meta      s)]
                    `(do (in-ns  '~(symbol ns))
                         (declare ~(with-meta (symbol v) m))))) syms)
         (in-ns '~(symbol original-ns)))))

(defmacro defalias
  "Defines an alias for a var, preserving metadata. Adapted from
  `clojure.contrib/def.clj`, Ref. http://goo.gl/xpjeH"
  ;; TODO Would be nice to have a ClojureScript impln. (impossible?)
  ([    target          ] `(defalias ~(symbol (name target)) ~target nil))
  ([sym target          ] `(defalias ~sym                    ~target nil))
  ([sym target docstring]
   `(let [^clojure.lang.Var v# (var ~target)]
      (alter-meta! (def ~sym (.getRawRoot v#))
        #(merge % (dissoc (meta v#) :column :line :file :test :name)
           (when-let [doc# ~docstring] {:doc doc#})))
      (var ~sym))))

(defmacro catch-errors*
  ;; Badly need something like http://dev.clojure.org/jira/browse/CLJ-1293
  ;; TODO js/Error instead of :default as temp workaround for http://goo.gl/UW7773
  ([try-form] `(catch-errors* ~try-form ~'_ nil))
  ([try-form error-sym error-form]
   `(if-cljs
      (try ~try-form (catch js/Error  ~error-sym ~error-form))
      (try ~try-form (catch Throwable ~error-sym ~error-form))))
  ([try-form error-sym error-form finally-form]
   `(if-cljs
      (try ~try-form (catch js/Error  ~error-sym ~error-form) (finally ~finally-form))
      (try ~try-form (catch Throwable ~error-sym ~error-form) (finally ~finally-form)))))

(defmacro catch-errors "Returns [<?result> <?error>]"
  [& body] `(catch-errors* [(do ~@body) nil] e# [nil e#]))

(defn str-starts-with? [s substr]
  #?(:clj  (.startsWith ^String s ^String substr)
     :cljs (zero? (.indexOf s substr))))

(defn without-meta [x] (if (meta x) (with-meta x nil) x))

(defmacro get-env []
  (let [ks (reduce
             (fn [acc in]
               (if (str-starts-with? (name in) "__") ; Hide privates
                 acc ; Strip primitive tags which can cause issues:
                 (conj acc (without-meta in))))
             [] (keys &env))]
    `(zipmap '~ks ~ks)))

(defn  now-udt ^long []
  #?(:clj (System/currentTimeMillis)
     :cljs (.getTime (js/Date.))))

(def ^:private ^:const gc-rate (/ 1.0 16000))

(defn gc-now? [] (<= ^double (rand) gc-rate))

;; ClojureScript keywords aren't `identical?` and Clojure doesn't have
;; `keyword-identical?`. This util helps alleviate the pain of writing
;; cross-platform code, Ref. http://goo.gl/be8CGP
#?(:cljs (def  kw-identical? keyword-identical?)
   :clj  (defn kw-identical?
           {:inline (fn [x y] `(. clojure.lang.Util identical ~x ~y))
            :inline-arities #{2}}
           ([x y] (clojure.lang.Util/identical x y))))

(defn swap-in!
  "More powerful version of `swap!`:
    * Supports optional `update-in` semantics.
    * Swap fn can return `(swapped <new-val> <return-val>)` rather than just
      <new-val>. This is useful when writing atomic pull fns, etc."

  ([atom_ f] ; Like `swap!` with `swapped` support
   (loop []
     (let [old-val @atom_
           [new-val return-val] (-swapped (f old-val))]
       (if (-platform-cas! atom_ old-val new-val)
         return-val
         (recur)))))

  ([atom_ ks f] ; Add `update-in` support
   (loop []
     (let [old-val @atom_
           [new-val return-val] (-swapped old-val ks f)]
       (if (-platform-cas! atom_ old-val new-val)
         return-val
         (recur)))))

  ;; Add `replace-in` support, note no way to support `swapped`
  ([atom_ ks f & more] {:pre [(assert (even? (count more)))]}
   (let [op-pairs (pairs-into [[ks f]] more)]
     (loop []
       (let [old-val @atom_
             new-val (-replace-in :swap old-val op-pairs)]
         (if (-platform-cas! atom_ old-val new-val)
           [old-val new-val]
           (recur)))))))

(defn rate-limiter* ; `rate-limiter` name taken by deprecated API
  "Takes one or more rate specs of form [ncalls-limit window-ms ?spec-id] and
  returns a (fn [& [req-id])) that returns `nil` (=> all rate limits passed), or
  [<ms-wait> <worst-offending-spec-id>] / <ms-wait>."
  [specs]
  (if (empty? specs)
    (constantly nil)
    (let [vspecs      (vec specs)
          vstates_    (atom {}) ; {<req-id> [[ncalls udt-window-start] <...>]}
          max-win-ms  (long (reduce max 0 (mapv (fn [[_ win-ms _ :as spec]] win-ms)
                                            vspecs)))
          nspecs      (count vspecs)
          nid-specs   (count (filterv (fn [[_ _ id]] id) vspecs))
          _           (assert (or (zero? nid-specs) (= nid-specs nspecs)))
          return-ids? (not (zero? nid-specs))]

      (fn check-rate-limits [& [?a1 ?a2]]
        (cond
          (kw-identical? ?a1 :rl/debug) vstates_
          (kw-identical? ?a1 :rl/reset)
          (do
            (if (kw-identical? ?a2 :rl/all)
              (reset! vstates_ {})
              (swap!  vstates_ dissoc ?a2))
            nil)

          :else
          (let [peek?   (kw-identical? ?a1 :rl/peek)
                req-id  (if peek? ?a2 ?a1)
                instant (now-udt)]

            (when (and req-id (gc-now?))
              (swap-in! vstates_ []
                (fn gc [m]
                  (reduce-kv
                    (fn [m* req-id vstate]
                      (let [^long max-udt-win-start
                            (reduce (fn [^long acc [_ ^long udt _]]
                                      (max acc udt))
                              0 vstate)
                            min-win-ms-elapsed (- instant max-udt-win-start)]
                        (if (> min-win-ms-elapsed max-win-ms)
                          (dissoc m* req-id)
                          m*)))
                    m m))))

            (swap-in! vstates_ [req-id]
              (fn [?vstate]
                (if-not ?vstate
                  (if peek?
                    (swapped ?vstate nil)
                    (swapped (vec (repeat nspecs [1 instant])) nil))

                  ;; Need to atomically check if all limits pass before committing
                  ;; to any ncall increments:
                  (let [[vstate-with-resets ?worst-limit-offence]
                        (loop [in-vspecs  vspecs
                               in-vstate  ?vstate
                               out-vstate []
                               ?worst-limit-offence nil]
                          (let [[[^long ncalls-limit ^long win-ms ?spec-id]
                                 & next-in-vspecs] in-vspecs
                                [[^long ncalls ^long udt-win-start]
                                 & next-in-vstate] in-vstate

                                win-ms-elapsed (- instant udt-win-start)
                                reset-due?     (>= win-ms-elapsed win-ms)
                                rate-limited?  (and (not reset-due?)
                                                    (>= ncalls ncalls-limit))
                                new-out-vstate ; No ncall increments yet:
                                (conj out-vstate
                                  (if reset-due? [0 instant] [ncalls udt-win-start]))

                                new-?worst-limit-offence
                                (if-not rate-limited?
                                  ?worst-limit-offence
                                  (let [ms-wait (- win-ms win-ms-elapsed)]
                                    (if (or (nil? ?worst-limit-offence)
                                            (let [[^long max-ms-wait _] ?worst-limit-offence]
                                              (> ms-wait max-ms-wait)))
                                      [ms-wait ?spec-id]
                                      ?worst-limit-offence)))]

                            (if-not next-in-vspecs
                              [new-out-vstate new-?worst-limit-offence]
                              (recur next-in-vspecs next-in-vstate new-out-vstate
                                     new-?worst-limit-offence))))

                        all-limits-pass? (nil? ?worst-limit-offence)
                        new-vstate
                        (cond
                          peek? ?vstate
                          (not all-limits-pass?) vstate-with-resets
                          :else
                          (mapv (fn [[^long ncalls udt-win-start]]
                                  [(inc ncalls) udt-win-start])
                            vstate-with-resets))

                        result
                        (when-let [wlo ?worst-limit-offence]
                          (if return-ids?
                            wlo
                            (let [[ms-wait _] wlo] ms-wait)))]

                    (swapped new-vstate result)))))))))))

(defn rsome "Faster `some` based on `reduce`"
  [pred coll] (reduce
               (fn [acc in] (when-let [p (pred in)] (reduced p)))
               nil coll))

(defn -nested-merge-with [f maps]
  (when (rsome identity maps) ; (merge nil nil) => nil
    (reduce
      (fn [acc in]
        (reduce-kv
          (fn rf2 [acc k rv]
            (if (contains? acc k)
              (let [lv (get acc k)]
                (if (and (map? lv) (map? rv))
                  (assoc acc k (reduce-kv rf2 lv rv))
                  (if (kw-identical? rv :merge/dissoc)
                    (dissoc acc k)
                    (let [new-rv (f lv rv)]
                      (if (kw-identical? new-rv :merge/dissoc)
                        (dissoc acc k)
                        (assoc  acc k new-rv))))))
              (assoc acc k rv)))
          acc
          in))
      {}
      maps)))

(defn nested-merge-with [f & maps] (-nested-merge-with f           maps))
(defn nested-merge        [& maps] (-nested-merge-with (fn [x y] y) maps))

(defn now-dt [] #?(:clj (java.util.Date.)
                   :cljs (js/Date.)))

(defn re-pattern? [x]
  (instance? #?(:cljs js/RegExp
                :clj java.util.regex.Pattern)
             x))

(defmacro thread-local-proxy "Ref. http://goo.gl/CEBJnQ (instant.clj)"
  [& body] `(proxy [ThreadLocal] [] (initialValue [] (do ~@body))))

#?(:clj
   (def ^:private -simple-date-format
    "Returns a SimpleDateFormat ThreadLocal proxy"
    (memo/lru
     (fn [^String pattern & [{:keys [locale timezone]}]]
       (let [^Locale locale
             (if (kw-identical? locale :jvm-default)
               (Locale/getDefault)
               locale)

             ^TimeZone timezone
             (case timezone
               :jvm-default (TimeZone/getDefault)
               :utc         (TimeZone/getTimeZone "UTC")
               timezone)]

         (thread-local-proxy
          (let [^SimpleDateFormat sdformat
                (if locale
                  (SimpleDateFormat. pattern locale)
                  (SimpleDateFormat. pattern))]
            (when timezone (.setTimeZone sdformat timezone))
            sdformat)))))))

#?(:clj
   (defn simple-date-format
    "Returns a thread-local `java.text.SimpleDateFormat`, Ref. http://goo.gl/Vh392A
    ~Prefer java.time (Java 8) > Joda-Time > Tower/DateFormat > SimpleDateFormat"
    ^java.text.SimpleDateFormat [pattern & [{:keys [locale timezone] :as opts}]]
    (let [pattern
          (case pattern
            :iso8601 "yyyy-MM-dd HH:mm:ss.SSSZ"
            :rss2 "EEE, dd MMM yyyy HH:mm:ss z"
            pattern)]
      (.get ^ThreadLocal (-simple-date-format pattern opts)))))

(def str-builder "For cross-platform string building"
  #?(:clj (fn (^StringBuilder []       (StringBuilder.))
            (^StringBuilder [s-init] (StringBuilder. ^String s-init)))
     :cljs (fn ([]       (goog.string.StringBuffer.))
             ([s-init] (goog.string.StringBuffer. s-init)))))

(defn sb-append "For cross-platform string building"
  #?(:clj (^StringBuilder [^StringBuilder str-builder ^String s]
                          (.append str-builder s))
     :cljs ([str-builder s] (.append str-builder s)))
  ([str-builder s & more]
   (sb-append str-builder s)
   (reduce (fn [acc in] (sb-append acc in)) str-builder more)))

(defn str-builder? [x]
  #?(:clj (instance? StringBuilder x)
     :cljs (instance? goog.string.StringBuffer x)))

(def str-rf "String builder reducing fn"
  (fn
    ([]       (str-builder))
    ([acc]               (if (str-builder? acc) acc (str-builder (str acc))))
    ([acc in] (sb-append (if (str-builder? acc) acc (str-builder (str acc)))
                         (str in)))))

(defn str-join
  "Faster, transducer-based generalization of `clojure.string/join` with `xform`
   support"
  ([                coll] (str-join nil       nil coll))
  ([separator       coll] (str-join separator nil coll))
  ([separator xform coll]
   (if (and separator (not= separator ""))
     (let [sep-xform (interpose separator)
           str-rf*   (completing str-rf str)]
       (if xform
         (transduce (comp xform sep-xform) str-rf* coll)
         (transduce             sep-xform  str-rf* coll)))
     (if xform
       (transduce xform (completing str-rf str) coll)
       (str (reduce str-rf coll))))))

(defn vnext [v]
  (when (> (count v) 1) (subvec v 1)))

(defn vsplit-last [v]
  (let [c (count v)]
    (when (> c 0) [(when (> c 1) (pop v)) (peek v)])))

(defn vsplit-first [v]
  (let [c (count v)]
    (when (> c 0) (let [[v1] v] [v1 (when (> c 1) (subvec v 1))]))))
