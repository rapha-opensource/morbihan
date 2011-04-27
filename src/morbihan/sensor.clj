(ns morbihan.sensor (:use morbihan.model))

(defstruct sensor-state
           :explore
           :id
           :log
           :sample-id
           :sample2input
           :input2sample
           :predicted-sample
           :prediction-rate
           :predictions
           :current-pattern)

(defn sensor []
  (create-neuron (struct sensor-state
                         :sensor
                         nil               ;id
                         nil              ;:log
                         0                 ;sample-id
                         nil               ;sample2input
                         nil               ;input2sample
                         nil              ;predicted-sample
                         0                ;prediction-rate
                         0                ;predictions
                         (atom nil))))     ;current-pattern

(defn add-sample
  [{:keys [id current-pattern] :as oldstate} sample]
  (let [newstate  (inc-sample-id oldstate)
        sample-id (newstate :sample-id)]
    (reset! current-pattern [sample-id id])
    (assoc newstate :sample2input (assoc (newstate :sample2input) sample            @current-pattern)
                    :input2sample (assoc (newstate :input2sample) @current-pattern  sample))))

(defn reset-stats
  [oldstate]
  (assoc oldstate :predictions 0 :prediction-rate 0))

(defn update-stats
  [{:keys [predicted-sample predictions prediction-rate] :as oldstate} sample]
  (if (nil? predicted-sample)
    oldstate
    (let [newstate (assoc oldstate :prediction-rate (/ (+ (* prediction-rate predictions) 
                                                           (if (= predicted-sample sample) 1 0))
                                                        (+ predictions 1))
                                   :predictions (+ predictions 1))]
      (log newstate "{" (newstate :prediction-rate) "}"))))

(defn feel-input
  [{:keys [id sample2input current-pattern] :as oldstate} sample]
  (let [newstate (-> oldstate
                   (log "\n" sample ">")
                   (update-stats sample))]
    (if-let [input (get sample2input sample)]
      (do
        (when (not= @current-pattern input) (reset! current-pattern input))
        newstate)
      (add-sample newstate sample))))

(defn predicted-input
  [oldstate input]
  (assoc oldstate :predicted-sample (get (:input2sample oldstate) (second input))))

(defn replay
  [oldstate input]
  (log oldstate "\n" (get (:input2sample oldstate) (second input))">P" ))

(defn send-pattern
  [periods sensors-periods neurons]
  (println "____________________________________________________________________________________________\n")
  (println "     Begin to send a value to a sensor, then wait for network to settle and do it again...\n")
  (println "____________________________________________________________________________________________\n")
  (loop [ns (reduce concat () (repeat periods sensors-periods))]
    (when-not (nil? ns)
      (send (first ns) feel-input (second ns))
      ;(Thread/sleep 9)
      (apply await neurons)
      (apply pr-log neurons)
      (println "\n....................................")
      (apply erase-all-logs neurons)
      (apply await neurons)
      (recur (nnext ns)))))

