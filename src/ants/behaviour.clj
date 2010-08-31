;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ant sim ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;   which can be found in the file CPL.TXT at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;dimensions of square world
(ns ants.behaviour
  (:require (ants [world :as w]
		  [defs :as defs])))

(def running true)

(defn turn 
  "turns the ant at the location by the given amount"
  [loc amt]
  (dosync
   (let [p (w/place loc)
	 ant (:ant @p)]
     (alter p assoc :ant (assoc ant :dir (defs/bound 8 (+ (:dir ant) amt))))))
  loc)

(defn move 
  "moves the ant in the direction it is heading. Must be called in a
  transaction that has verified the way is clear"
  [loc]
     (let [oldp (w/place loc)
           ant (:ant @oldp)
           newloc (defs/delta-loc loc (:dir ant))
           p (w/place newloc)]
;move the ant
       (alter p assoc :ant ant)
       (alter oldp dissoc :ant)
;leave pheromone trail
       (when-not (:home @oldp)
         (alter oldp update-in [:pher] inc))
       newloc))

(defn take-food [loc]
  "Takes one food from current location. Must be called in a
  transaction that has verified there is food available"
  (let [p (w/place loc)
        ant (:ant @p)]    
    (alter p assoc 
           :food (dec (:food @p))
           :ant (assoc ant :food true))
    loc))

(defn drop-food [loc]
  "Drops food at current location. Must be called in a
  transaction that has verified the ant has food"
  (let [p (w/place loc)
        ant (:ant @p)]    
    (alter p assoc 
           :food (inc (:food @p))
           :ant (dissoc ant :food))
    loc))

(defn rank-by 
  "returns a map of xs to their 1-based rank when sorted by keyfn"
  [keyfn xs]
  (let [sorted (sort-by (comp float keyfn) xs)]
    (reduce (fn [ret i] (assoc ret (nth sorted i) (inc i)))
            {} (range (count sorted)))))



(defn behave 
  "the main function for the ant agent"
  [loc]
  (let [p (w/place loc)
        ant (:ant @p)
        ahead (w/place (defs/delta-loc loc (:dir ant)))
        ahead-left (w/place (defs/delta-loc loc (dec (:dir ant))))
        ahead-right (w/place (defs/delta-loc loc (inc (:dir ant))))
        places [ahead ahead-left ahead-right]]
    (dosync
     (when running
       (send-off *agent* #'behave))
     (. Thread (sleep defs/ant-sleep-ms))
     (if (:food ant) ;going home
       (cond
	 (:home @p)
	  (-> loc drop-food (turn 4))
	 (and (:home @ahead) (not (:ant @ahead))) 
	  (move loc)
	 :else
	  (let [ranks (merge-with + 
		        (rank-by #(if (:home @%) 1 0) places)
			(rank-by #(:pher @%) places))]
	    (defs/wrand-choice
	      (if (:ant @ahead) 0 (ranks ahead))
                (move loc)
	      (ranks ahead-left)
		(turn loc -1)
	      (ranks ahead-right)
	        (turn loc 1))))
       
		 
       (cond ;foraging
        (and (pos? (:food @p)) (not (:home @p))) 
          (-> loc take-food (turn 4))
        (and (pos? (:food @ahead)) (not (:home @ahead)) (not (:ant @ahead)))
          (move loc)
        :else
          (let [ranks (merge-with + 
                        (rank-by #(:food @%) places)
			(rank-by #(:pher @%) places))
		weights [(if (:ant @ahead) 0 (ranks ahead)) 
			 (ranks ahead-left) 
			 (ranks ahead-right)]]
          (defs/wrand-choice
	      (if (:ant @ahead) 0 (ranks ahead))
                (move loc)
	      (ranks ahead-left)
		(turn loc -1)
	      (ranks ahead-right)
	        (turn loc 1))))))))

