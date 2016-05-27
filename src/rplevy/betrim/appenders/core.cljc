(ns rplevy.betrim.appenders.core
  "Core Betrim appenders without any special dependency requirements.
  These can be aliased into the main Betrim ns for convenience."
  {:author "Robert P. Levy"}
  (:require
   #?(:clj [clojure.core.memoize :as memo])
   [clojure.string :as str]
   [rplevy.betrim.utils :as ut]))

;;;; Println appender (clj & cljs)

#?(:clj (ut/declare-remote rplevy.betrim/default-out
                           rplevy.betrim/default-err))
#?(:clj (alias 'betrim 'rplevy.betrim))

(defn println-appender
  "Returns a simple `println` appender for Clojure/Script.
  Use with ClojureScript requires that `cljs.core/*print-fn*` be set.

  :stream (clj only) - e/o #{:auto :*out* :*err* :std-err :std-out <io-stream>}."

  ;; Unfortunately no easy way to check if *print-fn* is set. Metadata on the
  ;; default throwing fn would be nice...

  [& #?(:clj [{:keys [stream] :or {stream :auto}}]) #?(:cljs [_opts])]
  (let [#?(:clj stream)
        #?(:clj (case stream
                  :std-err betrim/default-err
                  :std-out betrim/default-out
                  stream))]

    {:enabled?   true
     :async?     false
     :min-level  nil
     :rate-limit nil
     :output-fn  :inherit
     :fn
     (fn [data]
       (let [{:keys [output-fn]} data]
         #?(:cljs (println (output-fn data)))
         #?(:clj
            (let [stream (case stream
                           :auto  (if (:error? data) *err* *out*)
                           :*out* *out*
                           :*err* *err*
                           stream)]
              (binding [*out* stream] (println (output-fn data)))))))}))

;;;; Spit appender (clj only)

#?(:clj
   (def ^:private ensure-spit-dir-exists!
     (memo/ttl
      (fn [fname]
        (when-not (str/blank? fname)
          (let [file (java.io.File. ^String fname)
                dir  (.getParentFile (.getCanonicalFile file))]
            (when-not (.exists dir) (.mkdirs dir)))))
      :ttl/threshold 60000)))

#?(:clj
   (defn spit-appender
    "Returns a simple `spit` file appender for Clojure."
    [& [{:keys [fname] :or {fname "./betrim-spit.log"}}]]
    {:enabled?   true
     :async?     false
     :min-level  nil
     :rate-limit nil
     :output-fn  :inherit
     :fn
     (fn [data]
       (let [{:keys [output-fn]} data]
         (try ; To allow TTL-memoization of dir creator
           (ensure-spit-dir-exists! fname)
           (spit fname (str (output-fn data) "\n") :append true)
           (catch java.io.IOException _))))}))

;;;; js/console appender (cljs only)

#?(:cljs
   (defn console-appender
    "Returns a simple js/console appender for ClojureScript.

    For accurate line numbers in Chrome, add these Blackbox[1] patterns:
      `/rplevy/betrim/appenders/core\\.js$`
      `/rplevy/betrim\\.js$`
      `/cljs/core\\.js$`

    [1] Ref. https://goo.gl/ZejSvR"

    ;; TODO Any way of using something like `Function.prototype.bind`
    ;; (Ref. https://goo.gl/IZzkQB) to get accurate line numbers in all
    ;; browsers w/o the need for Blackboxing?

    [& [{:keys [raw-output?]} ; Undocumented (experimental)
        ]]
    {:enabled?   true
     :async?     false
     :min-level  nil
     :rate-limit nil
     :output-fn  :inherit
     :fn
     (if (and (exists? js/console) js/console.log)
       (let [;; Don't cache this; some libs dynamically replace js/console
             level->logger
             (fn [level]
               (or
                (case level
                  :trace  js/console.trace
                  :debug  js/console.debug
                  :info   js/console.info
                  :warn   js/console.warn
                  :error  js/console.error
                  :fatal  js/console.error
                  :report js/console.info)
                js/console.log))]

         (fn [data]
           (let [{:keys [level output-fn vargs_]} data
                 vargs      @vargs_
                 [v1 vnext] (ut/vsplit-first vargs)
                 logger     (level->logger level)]

             (if (or raw-output? (ut/kw-identical? v1 :betrim/raw))
               (let [output (output-fn (assoc data
                                              :msg_  (delay "")
                                              :?err_ (delay nil)))
                     args (->> vnext (cons @(:?err_ data)) (cons output))]

                 (.apply logger js/console (into-array args)))
               (.call    logger js/console (output-fn data))))))
       (fn [data] nil))}))
