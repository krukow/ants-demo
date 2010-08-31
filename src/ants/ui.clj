;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ant sim ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;   which can be found in the file CPL.TXT at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ants.ui
  (:require (ants [defs :as defs]
		  [world :as w]
		  [behaviour :as b]))
  (:import (java.awt Color Graphics Dimension)
	   (java.awt.image BufferedImage)
	   (javax.swing JPanel JFrame)))
  

;pixels per world cell
(def scale 5)

(defn fill-cell [#^Graphics g x y c]
  (doto g
    (.setColor c)
    (.fillRect (* x scale) (* y scale) scale scale)))

(defn render-ant [ant #^Graphics g x y]
  (let [black (. (new Color 0 0 0 255) (getRGB))
        gray (. (new Color 100 100 100 255) (getRGB))
        red (. (new Color 255 0 0 255) (getRGB))
        [hx hy tx ty] ({0 [2 0 2 4] 
                        1 [4 0 0 4] 
                        2 [4 2 0 2] 
                        3 [4 4 0 0] 
                        4 [2 4 2 0] 
                        5 [0 4 4 0] 
                        6 [0 2 4 2] 
                        7 [0 0 4 4]}
                       (:dir ant))]
    (doto g
      (.setColor (if (:food ant) 
                  (new Color 255 0 0 255) 
                  (new Color 0 0 0 255)))
      (.drawLine (+ hx (* x scale)) (+ hy (* y scale)) 
                (+ tx (* x scale)) (+ ty (* y scale))))))

(defn render-place [g p x y]
  (when (pos? (:pher p))
    (fill-cell g x y (new Color 0 255 0 
                          (int (min 255 (* 255 (/ (:pher p) defs/pher-scale)))))))
  (when (pos? (:food p))
    (fill-cell g x y (new Color 255 0 0 
                          (int (min 255 (* 255 (/ (:food p) defs/food-scale)))))))
  (when (:ant p)
    (render-ant (:ant p) g x y)))

;; snapshots the world and paints it in it's own time.
(defn render [g]
  (let [v (dosync (into [] (for [x (range defs/dim) y (range defs/dim)] 
                                   @(w/place [x y]))))
        img (new BufferedImage (* scale defs/dim) (* scale defs/dim) 
                 (. BufferedImage TYPE_INT_ARGB))
        bg (. img (getGraphics))]
    (doto bg
      (.setColor (. Color white))
      (.fillRect 0 0 (. img (getWidth)) (. img (getHeight))))
    (dorun 
     (for [x (range defs/dim) y (range defs/dim)]
       (render-place bg (v (+ (* x defs/dim) y)) x y)))
    (doto bg
      (.setColor (. Color blue))
      (.drawRect (* scale w/home-off) (* scale w/home-off) 
                (* scale defs/nants-sqrt) (* scale defs/nants-sqrt)))
    (. g (drawImage img 0 0 nil))
    (. bg (dispose))))

(def panel (doto (proxy [JPanel] []
                        (paint [g] (render g)))
             (.setPreferredSize (new Dimension 
                                    (* scale defs/dim) 
                                    (* scale defs/dim)))))

(def frame (doto (new JFrame) (.add panel) (.pack) (.show)))

(def animator (agent nil))

(defn animation [x]
  (when b/running
    (send-off *agent* #'animation))
  (. panel (repaint))
  (. Thread (sleep defs/animation-sleep-ms))
  nil)

(def evaporator (agent nil))

(defn evaporation [x]
  (when b/running
    (send-off *agent* #'evaporation))
  (w/evaporate)
  (. Thread (sleep defs/evap-sleep-ms))
  nil)

(defn start-anim [] (send-off animator animation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; use ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(comment
;demo
;;(load-file "/Users/krukow//Documents/talks/clojure/ants/src/ants/ui.clj")
(def ants (w/setup))
(start-anim)
(dorun (map #(send-off % b/behave) ants))
(send-off evaporator evaporation)

)
