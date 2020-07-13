(ns my-test1
  (:require [quil.core :as q]
            [quil.middleware :as m])
  (:gen-class)
  )
;---------------------------------------------------------------------------------------------
; Вычисление силы взаимодействия шариков используя потенциал Ленарда-Джонса
; Calculation of the force of interaction of balls using the Lenard-Jones potential
; e - the depth of the potential yawner
; r - the distance between the centers of the particles
; q - the distance at which the interaction energy becomes zero
(defn LennardJonesPower [e q r]
  (if (> r 0)
    (if (<= r (* (Math/pow 2 (/ 1 6)) q))
      -5
      (if (< r (* 2.5 q)) 
        (* (/ (* 12 e) q)(-(Math/pow (/ q r) 13)(Math/pow (/ q r) 7)))  
        (* e 0.02)
        )
      )
    0
    )
  )
;---------------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------------
; Вычисление скорости
(defn Speed [t_step v_current m_ball LJpow]
  (+ v_current (* (/ LJpow m_ball) t_step))
  )
;---------------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------------
; Вычисление координаты Х 
(defn newParticleCoordinate [t_step cord_current m_ball speed LJpow]
  (+ cord_current (* speed t_step) (* (/ LJpow m_ball) (/(Math/pow t_step 2)2)))
  )
;---------------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------------
(defn funс []
  
  (doseq [[i] (map list (range(count particleData)))]
    (doseq [[j] (map list (range(count particleData)))]  
      (def particleDataCopy (assoc (particleData i) :curLJPower (+ ((particleData i) :curLJPower) 
                                                                   (LennardJonesPower (Data 2) (Data 3) 
                                                                                      (Math/sqrt(+
                                                                                                  (Math/pow(-(((get (vec particleData) i):coord)0) (((get (vec particleData) j):coord)0))2)
                                                                                                  (Math/pow(-(((get (vec particleData) i):coord)1) (((get (vec particleData) j):coord)1))2)
                                                                                                  (Math/pow(-(((get (vec particleData) i):coord)2) (((get (vec particleData) j):coord)2))2)))
                                                                                      ))))
      (def particleData (assoc particleData i particleDataCopy))
      )
    )
  
  (doseq [[i] (map list (range(count particleData)))]
    (def particleDataCopy (assoc (particleData i) :curSpeed (Math/abs (Speed (Data 0) ((particleData i) :curSpeed) 
                                                                             ((particleData i) :mass) ((particleData i) :curLJPower)))
                                 :coord  [(newParticleCoordinate (Data 0) (((particleData i):coord)0) ((particleData i) :mass) ((particleData i) :curSpeed) ((particleData i) :curLJPower))
                                          (newParticleCoordinate (Data 0) (((particleData i):coord)1) ((particleData i) :mass) ((particleData i) :curSpeed) ((particleData i) :curLJPower))
                                          (newParticleCoordinate (Data 0) (((particleData i):coord)2) ((particleData i) :mass) ((particleData i) :curSpeed) ((particleData i) :curLJPower))]))      
    (def particleData (assoc particleData i particleDataCopy))
    )   
  
  (doseq [[i] (map list (range(count particleData)))]   
    (def particleDataCopy (assoc (particleData i) 
                                 :curLJPower 0
                                 ))
    (def particleData (assoc particleData i particleDataCopy))
    )
  
  )
;---------------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------------
(defn draw [state]
  
  (let [t (/ (q/frame-count) 25)]
    
    (q/clear)
    (q/camera 50 50 50 0 0 0 0 0 -1)
    ; draw red X axis
    (q/stroke 255 0 0)
    (q/line 0 0 0 1000 0 0)
    ; draw green Y axis
    (q/stroke 0 255 0)
    (q/line 0 0 0 0 1000 0)
    ; draw blue Z axis
    (q/stroke 0 0 255)
    (q/line 0 0 0 0 0 1000)
    
    (q/sphere-detail 15)
    (q/with-translation ((particleData 0) :coord)
      (q/sphere ((particleData 0) :mass)))
    
    (q/sphere-detail 15)
    (q/with-translation ((particleData 1) :coord)
      (q/sphere ((particleData 1) :mass)))
    
    (q/sphere-detail 15)
    (q/with-translation ((particleData 2) :coord)
      (q/sphere ((particleData 2) :mass)))
    
    (q/sphere-detail 15)
    (q/with-translation ((particleData 3) :coord)
      (q/sphere ((particleData 3) :mass)))
    
    (funс)
    )
  )
;---------------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------------
(defn setup []
  (q/color-mode :rgb)
  (q/frame-rate 60)
  (def particleData [
                     {
                      :coord[10 8 0]
                      :mass 5
                      :curLJPower 0
                      :curSpeed 0
                      }
                     {
                      :coord[35 20 10]
                      :mass 5
                      :curLJPower 0
                      :curSpeed 0
                      }
                     {
                      :coord[7 4 13]
                      :mass 5
                      :curLJPower 0
                      :curSpeed 0
                      }
                     
                     {
                      :coord[11 24 11]
                      :mass 5
                      :curLJPower 0
                      :curSpeed 0
                      }
                     ]){})
;---------------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------------
(defn -main [x]
  (def Data x)
  (q/defsketch quil-experiments
    :size [500 500]
    :setup setup
    :draw draw 
    :middleware [m/fun-mode]
    :renderer :opengl))

;(-main [1 0 100 65])