(ns morbihan.model(:gen-class))
;  (teuron, seuron and sensor share some fields in the structmaps
;  that store the state of their agents.
;  :explore
;  :id
;  :sample-id
;  :current-pattern
;  :patterns
;  :tagged-pats-i0
;  :pred-o
;
;  Also, each type expects the following functions to be available:
;  on-new-pred-o
;
;  Some functions defined here are helpers to access these fields

(defstruct input-basis
                      :i0
                      :i1
                      :pred-count)

(defn reset []
  (def all-neurons (atom nil))
  (def watchers-id (atom 0))
  (def agent-id-gen (atom 0)))

(defn log
  [oldstate & more]
  (assoc oldstate :log (reduce str (oldstate :log) more)))

(defn log-header
  [oldstate]
  (log oldstate "\n=====" (oldstate :explore) ":" (oldstate :id) "======"))

(defn erase-log
  [oldstate]
  (log-header
    (assoc oldstate :log nil)))

(defn erase-all-logs
  [& neurons]
  (dorun (map #(send %1 erase-log) neurons)))

(defn pr-log
  [& neurons]
  (dorun (map #(print (:log (deref %1))) neurons)))

(defn create-neuron [args]
  (swap! agent-id-gen inc)
  (let [ this-agent (agent (-> args 
                             (assoc :id @agent-id-gen)
                             (log-header)))]
    (swap! all-neurons assoc @agent-id-gen this-agent)
    (add-watch (@this-agent :current-pattern) (gensym) (fn [k r o n] (send this-agent log "-|->" n "\n")))
    this-agent))

(defn patterns-with
  [patterns k input]
  (filter #(= input (k (val %))) patterns ))

(defn find-patterns
  ([patterns input1]
    (patterns-with patterns :i0 input1))
  ([patterns input1 input2]
   (-> (find-patterns patterns input1) (patterns-with :i1 input2) ffirst)))

(defn inc-sample-id
  [{:keys [sample-id] :as oldstate}]
  (assoc oldstate :sample-id (inc sample-id)))

(defn set-current-pattern
  [{:keys [id sample-id current-pattern] :as oldstate}]
  (let [output [id sample-id]]
    (when (not= @current-pattern output) (reset! current-pattern output))
    oldstate))

;(defn set-current-pattern
  ;[{:keys [id explore sample-id current-pattern] :as oldstate}]
  ;(let [output [id sample-id]]
    ;(reset! current-pattern output)
    ;oldstate))

(defn pred-o
  [neuron-agent]
  @(@neuron-agent :pred-o))

(defn pred-i0
  [neuron-agent]
  @(@neuron-agent :pred-i0))

(defn cur-pat
  [neuron-agent]
  @(@neuron-agent :current-pattern))

(defn fi ;i0
  [neuron-agent pattern-id]
  (:i0 (get (:patterns @neuron-agent) pattern-id)))

(defn si ;second-input
  [neuron-agent pattern-id]
  (:i1 (get (:patterns @neuron-agent) pattern-id)))

(defn set-i0
  [oldstate new-input]
  (-> oldstate (log "\n" new-input ">i0") (assoc :i0 new-input)))

(defn change-pattern
  ([oldstate pat-id pat-input-basis]
   (assoc oldstate :patterns (assoc (oldstate :patterns) pat-id pat-input-basis)))
  ([oldstate pat-id k v]
   (change-pattern oldstate pat-id (assoc (get (oldstate :patterns) pat-id) k v))))

(defn add-pattern
  [{:keys [id patterns] :as oldstate} first-input second-input]
    (let [newstate (inc-sample-id oldstate)
          sample-id (newstate :sample-id)]
      (change-pattern newstate [id sample-id]
                                    (struct input-basis first-input second-input 0))))

(defn change-patterns
  ([oldstate fun]
   (assoc oldstate :patterns (fun (oldstate :patterns))))
  ([oldstate fun & more]
   (assoc oldstate :patterns (apply fun (oldstate :patterns) more))))

;____________ Predictions related functions _____________________
;
(defn mod-pred-count
  ([fun input]
    (assoc input :pred-count (fun (:pred-count input))))
  ([fun patterns pat]
    (assoc patterns
             pat (mod-pred-count fun (patterns pat)))))

(defn inc-pred-count
  ([item]
    (assoc item 1 (mod-pred-count inc (second item))))
  ([patterns pat]
   (mod-pred-count inc patterns pat)))

(defn dec-pred-count
  ([item]
    (assoc item 1 (mod-pred-count dec (second item))))
  ([patterns pat]
    (mod-pred-count dec patterns pat)))

(defn tag-pattern
  [predicted-input fun pattern]
    (if (= predicted-input (:i0 (second pattern)))
      (fun pattern)
      pattern))

(defn inc-pattern
  [predicted-input pattern]
  (tag-pattern predicted-input inc-pred-count pattern))

(defn dec-pattern
  [predicted-input pattern]
  (tag-pattern predicted-input dec-pred-count pattern))

(defn get-pred-count
  ([item] (-> item second :pred-count))
  ([patterns pattern-id] (:pred-count (get patterns pattern-id))))

(defn get-pred-count-2
  "like get-pred-count but adapted to one item from output of find-patterns"
  ([item] (-> item first second :pred-count)))

(defn pattern-pred-count
  [neuron pattern-id] (get-pred-count (@neuron :patterns) pattern-id))

(defn pattern-pred-count-d
  [neuron pattern-id] (get-pred-count (neuron :patterns) pattern-id))

(defn most-probable-pattern
  [ patterns ]
  (let [pat (apply max-key get-pred-count patterns)]
    (if (< 0 (:pred-count (second pat))) pat nil)))

(defn most-probable-pattern-2
  "like most-probable-pattern, but for a find-pattern output"
  [ found-patterns ]
  (apply max-key get-pred-count-2 found-patterns))

(defn update-pred [old new-val]
  (list (second old) new-val))  
  
(defmulti update-pred-o (fn [state] (state :explore)))

(defn set-predictions
  ([ pred-o val-o pred-i0 val-i0 ]
    (when (not= @pred-o val-o)
      (reset! pred-o val-o))
    (when (not= (second @pred-i0) val-i0)
      (swap! pred-i0 update-pred val-i0)))
  ([pred-o val-o pred-i0 val-i0 pred-i1 val-i1]
   (set-predictions pred-o val-o pred-i0 val-i0)
   (when (not= (second @pred-i1) val-i1)
    (swap! pred-i1 update-pred val-i1))))

(defn reset-predictions
  ([pred-o pred-i0]
    (set-predictions pred-o nil pred-i0 nil))
  ([pred-o pred-i0 pred-i1]
    (reset-predictions pred-o pred-i0)
    (when (not= @pred-i1 nil) (reset! pred-i1 nil))))

(defn on-new-pred-o
  [oldstate pred-pat]
  (update-pred-o
    (let [newstate (if-let [new-pred (second pred-pat)]
                     (change-patterns oldstate inc-pred-count new-pred)
                     oldstate)]
       (if-let [old-pred (first pred-pat)]
          (change-patterns newstate dec-pred-count old-pred)
           newstate))))

(defn mod-pats
  [oldstate tagged-pats-field fun tagged-pats]
  (assoc (change-patterns oldstate fun)
         tagged-pats-field (if tagged-pats (list tagged-pats) (list))))

(defn inc-n-store
  [oldstate tagged-pats-field pat]
  (mod-pats oldstate tagged-pats-field #(inc-pred-count % pat) pat))

(defn inc-n-store-i0
  [oldstate pat]
  (inc-n-store oldstate :tagged-pats-i0 pat))

(defn inc-n-store-i1
  [oldstate pat]
  (inc-n-store oldstate :tagged-pats-i1 pat))

(defn untag-pats
  [oldstate tagged-pats-field]
  (let [tagged-pats (oldstate tagged-pats-field)]
    (mod-pats oldstate tagged-pats-field #(reduce dec-pred-count % tagged-pats) nil)))

(defn untag-pats-i0
  [oldstate]
  (untag-pats oldstate :tagged-pats-i0))

(defn untag-pats-i1
  [oldstate]
  (untag-pats oldstate :tagged-pats-i1))

(defn tag-one-pat
  [oldstate tagged-pats-field input-field i pattern]
  (if (= i (input-field (val pattern)))
    (let [pat-id (key pattern)]
          (assoc (change-patterns oldstate #(inc-pred-count % pat-id))
                 tagged-pats-field (conj (oldstate tagged-pats-field) pat-id)))
    oldstate))

(defn tag-one-pat-i0
  [oldstate i pattern]
  (tag-one-pat oldstate :tagged-pats-i0 :i0 i pattern))

(defn tag-one-pat-i1
  [oldstate i pattern]
  (tag-one-pat oldstate :tagged-pats-i1 :i1 i pattern))
