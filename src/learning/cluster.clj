(ns learning.cluster
  (:require [clojure.core.matrix :as mat]))

(defn k-means
  "Performs k-means clustering with first data points as.
   Returns empty vector after 5 restarts."
  ([data k]
     (letfn [(k-rand-nth [data k]
               (loop [rand-sel (take k (repeatedly #(rand-nth data)))]
                 (if (= (count (set rand-sel)) k)
                   rand-sel
                   (recur (take k (repeatedly #(rand-nth data)))))))]
       (loop [i 0 res (k-means data k (k-rand-nth data k))]
         (if (< (count (filter identity res)) k)
           (if (< i 5)
             (recur (inc i) (k-means data k (k-rand-nth data k)))
             [])
           res))))
  ([data k first-centroids]
     (letfn [(closest-ind [pt ptlist]
               (apply min-key #(euclidean-squared pt (nth ptlist %))
                      (range (count ptlist))))
             (average [ptlist]
               (if (< (count ptlist) 1) nil
                   (map #(/ % (count ptlist)) (apply map + ptlist))))
             (euclidean-squared [p1 p2]
               (reduce #(+ %1 (* %2 %2)) 0 (map - p1 p2)))
             (update-centroids [data labels]
               (map average (persistent!
                             (reduce #(assoc! %1 (nth labels %2)
                                              (cons (nth data %2) (get %1 (nth labels %2))))
                                     (transient (vec (take (count (set labels)) (repeat []))))
                                     (range (count data))))))]
       (loop [old-labels (take (count data) (repeat -1))
              centroids first-centroids]
         (let [new-labels (map #(closest-ind % centroids) data)]
           (if (not= old-labels new-labels)
             (recur new-labels (update-centroids data new-labels))
             centroids))))))

;(def data [[2 2] [2 4] [2 1] [0 1] [1 0] [2 0]])
;(def c [[0 0] [2 2]])
;(k-means data 2 c) ;;([5/4 1/2] [2 3])
