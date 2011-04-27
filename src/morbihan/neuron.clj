(ns morbihan.neuron
  (:use morbihan.model morbihan.sensor))

(defstruct teuron-state
           :log
           :explore
           :id
           :sample-id           
           :input-sequence-state
           :i0         
           :patterns
           :current-pattern
           :pred-o
           :pred-i0)

(defn teuron []
  (let [new-teuron (create-neuron (struct teuron-state
                         nil
                         :teuron
                         nil               ; :id
                         0                 ; :sample-id
                         :on-first-input   ; :input-sequence-state
                         nil               ; :i0
                         nil               ; :patterns
                         (atom nil)        ; :current-pattern
                         (atom nil) ; pred-o
                         (atom '(nil nil)) ; pred-i0
                         ))]
    (add-watch (@new-teuron :pred-o) (gensym) (fn [k r o n] (send new-teuron log "\n-P->" n)))
    (add-watch (@new-teuron :pred-i0)(gensym) (fn [k r o n] (send new-teuron log "\n" (second n) ">Pi0")))
    new-teuron))

(defn -iss
  [neuron-agent]
  (@neuron-agent :input-sequence-state))

(defn set-iss
  [oldstate new-state]
  (assoc oldstate :input-sequence-state new-state))

(defn dispatch-on-sequence-state
  ([oldstate]       (oldstate :input-sequence-state ))
  ([oldstate & more](dispatch-on-sequence-state oldstate)))

(defmulti feel            dispatch-on-sequence-state)
(defmulti find-new-pred-o dispatch-on-sequence-state)
(defmulti tag-pats        dispatch-on-sequence-state)
(defmulti new-pred-i0     dispatch-on-sequence-state)

(defmethod find-new-pred-o :on-new-pattern
  [oldstate]
  nil)

(defmethod find-new-pred-o :on-second-input
  [{:keys [patterns i0] :as oldstate}]
  {:pre [(not (nil? oldstate))]}
  (most-probable-pattern-2 (find-patterns patterns i0)))

(defmethod find-new-pred-o :on-first-input
  [{patterns :patterns}]
  (most-probable-pattern patterns))

(def state-2-input-key
  { :on-first-input :i0,
    :on-second-input :i1,
    :on-new-pattern  nil})

(defn set-predictions-with
  [pred-o pred-i0 pattern state]
   (set-predictions 
     pred-o (first pattern)
       pred-i0 ((state-2-input-key state) (second pattern))))

(defmethod update-pred-o :teuron
  [{:keys [input-sequence-state pred-o pred-i0] :as oldstate}]
   (if-let [max-pred-patt (find-new-pred-o oldstate)]
      (set-predictions-with pred-o pred-i0 max-pred-patt input-sequence-state)
      (reset-predictions pred-o pred-i0))
   oldstate)

(defn add-listener
  [source listener];listener HAS to be a teuron. Need to add a check
  (add-watch (@source   :current-pattern) (gensym) (fn [k r o n] (send listener  feel          n)))
  (add-watch (@listener :pred-i0)         (gensym) (fn [k r o n] (send source    on-new-pred-o n))))

(defn add-sensor
  [sensor listener]
  (add-watch (@sensor   :current-pattern) (gensym) (fn [k r o n] (send listener  feel             n)))
  (add-watch (@listener :pred-i0)         (gensym) (fn [k r o n] (send sensor    predicted-input  n)))
  (add-watch (@listener :pred-i0)         (gensym) (fn [k r o n] (send sensor    replay           n))))

(defmethod feel :on-first-input
  [{:keys [current-pattern patterns] :as oldstate} input]
  (let [newstate0 (set-i0 oldstate input)]
    (if (empty? (find-patterns patterns input))
       (do
         (reset-predictions (newstate0 :pred-o) (newstate0 :pred-i0))
         (-> newstate0 
           (add-pattern (newstate0 :i0) nil)
           (set-current-pattern )
           (set-iss :on-new-pattern)))
       (-> newstate0
         (set-iss :on-second-input)
         (update-pred-o)))))

(defmethod feel :on-new-pattern
  [{:keys [current-pattern] :as oldstate} input]
  (-> oldstate
    (log "\n" input ">i1")
    (change-pattern @current-pattern :i1 input)
    (set-iss  :on-first-input)))

(defmethod feel :on-second-input
  [{:keys [current-pattern i0 patterns pred-o pred-i0] :as oldstate} input]
  (reset-predictions pred-o pred-i0)
  (-> (if-let [pat (find-patterns patterns i0 input)]
        (do
          (when (not= @current-pattern pat) (reset! current-pattern pat))
          oldstate)
        (set-current-pattern (add-pattern oldstate i0 input)))
      (log "\n" input ">i1")
      (set-iss  :on-first-input)))
