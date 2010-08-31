;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ant sim ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;   which can be found in the file CPL.TXT at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ants.world
  (:require (ants [defs :as defs])))

(defrecord Cell [food pher]) ;may also have :ant and :home
(defrecord Ant [dir]) ;may also have :food 

;world is a 2d vector of refs to cells
(def world 
     (into [] (for [x (range defs/dim)]
		(into [] (for [y (range defs/dim)] 
			   (ref (Cell. 0 0)))))))

(defn place
  "get the location x,y in the world"
  [[x y]] ((world x) y))

(defn create-ant 
  "create an ant at the location, returning an ant agent on the location"
  [loc dir]
  (dosync
   (alter (place loc) assoc :ant (Ant. dir))
   (agent loc)))

(def home-off (/ defs/dim 4))
(def home-range (range home-off (+ defs/nants-sqrt home-off)))

(defn setup 
  "places initial food and ants, returns seq of ant agents"
  []
  (dosync
   (dotimes [i defs/food-places]
     (let [rnd-place (place [(rand-int defs/dim) 
			     (rand-int defs/dim)])]
       (alter rnd-place assoc :food (rand-int defs/food-range))))
   (doall
    (for [x home-range y home-range]
      (let [point [x y]]
	(alter (place point) assoc :home true)
	(create-ant point (rand-int 8)))))))

(defn evaporate 
  "causes all the pheromones to evaporate a bit"
  []
  (dorun 
   (for [x (range defs/dim) y (range defs/dim)]
     (dosync 
      (let [p (place [x y])
	    new-pher (* defs/evap-rate (:pher @p))]
        (alter p assoc :pher new-pher))))))
