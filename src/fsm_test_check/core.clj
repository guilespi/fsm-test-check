(ns fsm-test-check.core
  (:require [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.rose-tree :as rose]))

(defprotocol Command
  (precondition [this state] "Returns true if command can be applied in current system state")
  (postcondition [this state cmd] "Returns true if cmd can be applied on specified state")
  (exec [this state cmd] "Applies command in the specified system state, returns new state")
  (generate [this state] "Generates command given the current system state, returns command"))


(defn valid-sequence?
  [commands state-seq cmd-seq sub-seq-idxs]
  (when (seq sub-seq-idxs)
    (map? (reduce (fn [curr-state state-idx]
                       (let [cmd (get cmd-seq state-idx)
                             command (get commands (:type cmd))]
                         (if (postcondition command curr-state cmd)
                           (exec command curr-state cmd)
                           (reduced false))))
                     (first state-seq)
                     sub-seq-idxs))))

(defn remove-seq
  [s]
  (map-indexed (fn [index _]
                 (#'clojure.test.check.rose-tree/exclude-nth index s))
               s))

(defn shrink-sequence
  [cmd-seq state-seq commands]
  (letfn [(shrink-subseq [s]
            (when (seq s)
              (#'clojure.test.check.rose-tree/make-rose
               (map #(get cmd-seq %) s)
               (->> (remove-seq s)
                    (filter (partial valid-sequence? commands state-seq cmd-seq))
                    (mapv shrink-subseq)))))]
    (shrink-subseq (range 0 (count cmd-seq)))))

(defn cmd-seq-helper
  [state commands size]
  (gen/bind (gen/one-of (->> (map second commands)
                             (filter #(precondition % state))
                             (map #(generate % state))))
            (fn [cmd]
              (if (zero? size)
                (gen/return [[cmd state]])
                (gen/fmap
                 (partial concat [[cmd state]])
                 (cmd-seq-helper (exec (get commands (:type cmd)) state cmd)
                                 commands
                                 (dec size)))))))

(defn cmd-seq
  [state commands]
  (gen/bind (gen/choose 0 5)
            (fn [num-elements]
              (gen/bind (cmd-seq-helper state commands num-elements)
                        (fn [cmd-seq]
                          (-> (shrink-sequence (mapv first cmd-seq)
                                               (mapv second cmd-seq)
                                               commands)
                              gen/gen-pure))))))

;;-----------------------------------------------------
;;commands

(def add-cmd
  (reify
    Command
    (precondition [_ state]
      (vector? (:people state)))

    (postcondition [_ state cmd]
      ;;add only valid if no other person with same id
      (->> (:people state)
           (filter #(= (:id %) (:id cmd)))
           seq
           nil?))

    (exec [_ state cmd]
      (update-in state [:people] (fn [people]
                                   (conj people
                                         (dissoc cmd :type)))))

    (generate [_ state]
          (gen/fmap (partial zipmap [:type :name :id])
                    (gen/tuple (gen/return :add-cmd)
                               (gen/not-empty gen/string-alphanumeric)
                               (gen/such-that #(-> (mapv :id (:people state))
                                                   (contains? %)
                                                   not)
                                              gen/int))))))

(def delete-cmd
  (reify
    Command
    (precondition [_ state]
      (seq (:people state)))

    (postcondition [_ state cmd]
      ;;delete only valid if existing person with id
      (->> (:people state)
           (filter #(= (:id %) (:id cmd)))
           seq))

    (exec [_ state cmd]
      (update-in state [:people] (fn [people]
                                   (vec (filter #(not= (:id %)
                                                       (:id cmd))
                                                people)))))

    (generate [_ state]
      (gen/fmap (partial zipmap [:type :id])
                (gen/tuple (gen/return :delete-cmd)
                           (gen/elements (mapv :id (:people state))))))))

;;-----------------------------------------------------
;;property definition

(defn apply-tx
  "Apply transactions fails when there are two delete commands"
  [tx-log]
  (->> tx-log
       (filter #(= :delete-cmd (:type %)))
       count
       (> 2)))

(def commands-consistent-apply
  (prop/for-all [tx-log (cmd-seq {:people []} {:add-cmd add-cmd :delete-cmd delete-cmd})]
                (true? (apply-tx tx-log))))

(comment
  (tc/quick-check 100 commands-consistent-apply)
  )
