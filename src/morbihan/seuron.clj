(ns morbihan.seuron (:use morbihan.model morbihan.sensor))
(comment
  "Interface via duck-typing: connecting sources must have a :current-pattern reference
  to watch.
  Public API:
  seuron [] (returns a seuron agent)
  add-sources [watcher i0 i1] (attaches the seuron watcher to 2 inputs sources)
  (for testing only:)
  on-new-input [oldstate input key] (sends an input to either :i0 or :i1)"
)

(defstruct seuron-state
 :log
 :explore
 :id
 :sample-id
 :i0
 :i1
 :patterns
 :current-pattern
 :pred-o
 :pred-i0
 :tagged-pats-i0
 :pred-i1
 :tagged-pats-i1)

(defn seuron []
 (let [new-seuron (create-neuron (struct seuron-state
                        nil
                        :seuron
                        nil         ;id
                        0    ;sample-id
                        nil  ;i0
                        nil  ;i1
                        nil         ; :patterns
                        (atom nil)  ;current-pattern
                        (atom nil)  ;pred-o
                        (atom '(nil nil))  ;pred-i0
                        nil
                        (atom '(nil nil))  ;pred-i1
                        nil
                        ))]
  (add-watch (@new-seuron :pred-o)  (gensym) (fn [k r o n] (send new-seuron log "\n-P->" n)))
  (add-watch (@new-seuron :pred-i0) (gensym) (fn [k r o n] (send new-seuron log "\n" (second n) ">Pi0")))
  (add-watch (@new-seuron :pred-i1) (gensym) (fn [k r o n] (send new-seuron log "\n" (second n) ">Pi1")))
  new-seuron))

(defn set-i1
  [oldstate new-i1]
  (-> oldstate (log "\n" new-i1 ">i1") (assoc :i1 new-i1)))

(defn pred-i1
  [neuron-agent]
  @(@neuron-agent :pred-i1))

(defn find-n-set-current-pattern 
  [{:keys [i0 i1 current-pattern patterns] :as oldstate}]
    (if-let [pat-id (find-patterns patterns i0 i1)]
                 (do 
                   (when (not= @current-pattern pat-id)
                     (reset! current-pattern pat-id))
                   oldstate)
                 (if (and i0 i1)
                         (set-current-pattern (add-pattern oldstate i0 i1))
                         oldstate)))

(defn on-new-i0
  [oldstate new-i0]
  (let [newstate (set-i0 oldstate new-i0)]
    (find-n-set-current-pattern newstate)))

(defn on-new-i1
  [oldstate new-i1]
  (let [newstate (set-i1 oldstate new-i1)]
    (find-n-set-current-pattern newstate)))

(defmethod update-pred-o :seuron
  [{:keys [pred-o pred-i0 pred-i1 patterns] :as oldstate}]
  ;(when-let [max-pred-pat (most-probable-pattern patterns)]
    ;(let [new-o (first max-pred-pat)
          ;new-i0 (:i0 (second max-pred-pat))
          ;new-i1 (:i1 (second max-pred-pat))]
      ;(set-predictions pred-o new-o pred-i0 new-i0 pred-i1 new-i1)))
  (if-let [max-pred-pat (most-probable-pattern patterns)]
    (let [new-o (first max-pred-pat)
          new-i0 (:i0 (second max-pred-pat))
          new-i1 (:i1 (second max-pred-pat))]
      (set-predictions pred-o new-o pred-i0 new-i0 pred-i1 new-i1))
    (reset-predictions pred-o pred-i0 pred-i1))
  oldstate)

(defn add-sensors
  [sensor0 sensor1 seuron]
  (add-watch (@sensor0 :current-pattern)  (gensym) (fn [k r o n] (send seuron   on-new-i0 n)))
  (add-watch (@sensor1 :current-pattern)  (gensym) (fn [k r o n] (send seuron   on-new-i1 n)))
  (add-watch (@seuron :pred-i0)           (gensym) (fn [k r o n] (send sensor0  replay    n)))
  (add-watch (@seuron :pred-i0)           (gensym) (fn [k r o n] (send sensor0  predicted-input n)))
  (add-watch (@seuron :pred-i1)           (gensym) (fn [k r o n] (send sensor1  replay          n)))
  (add-watch (@seuron :pred-i1)           (gensym) (fn [k r o n] (send sensor1  predicted-input n))))

(defn add-sources
  [seuron-watcher i0 i1]
  ;[k r o n] parameters mean key, reference, old-state, new-state
  (add-watch (@i0 :current-pattern)     (gensym)(fn [k r o n] (send seuron-watcher  on-new-i0 n)))
  (add-watch (@seuron-watcher :pred-i0) (gensym)(fn [k r o n] (send i0              on-new-pred-o  n)))
  (add-watch (@i1 :current-pattern)     (gensym)(fn [k r o n] (send seuron-watcher  on-new-i1      n)))
  (add-watch (@seuron-watcher :pred-i1) (gensym)(fn [k r o n] (send i1              on-new-pred-o  n))))

