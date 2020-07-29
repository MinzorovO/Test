(ns my-test1.core 
  (:gen-class)
  (:require [quil.core :as q]
            [quil.middleware :as m])
  )
;---------------------------------------------------------------------------------------------
(defn setup []
  (q/color-mode :rgb)
  (q/frame-rate 10)
  (def particleData [
                     {
                      :coord[80 0 0]
                      :mass 15
                      :curSpeed 0
                      }
                     {
                      :coord[80 0 80]
                      :mass 15
                      :curSpeed 0
                      }
                     {
                      :coord[0 80 80]
                      :mass 15
                      :curSpeed 0
                      }
                     {
                      :coord[0 80 0]
                      :mass 15
                      :curSpeed 0
                      }
                     ]){})
;---------------------------------------------------------------------------------------------
(defn draw [state]  
  (q/clear)
  (q/camera 150 150 150 0 0 0 0 0 -1)
  ; draw red X axis
  (q/stroke 255 0 0)
  (q/line 0 0 0 1000 0 0)
  ; draw green Y axis
  (q/stroke 0 255 0)
  (q/line 0 0 0 0 1000 0)
  ; draw blue Z axis
  (q/stroke 0 0 255)
  (q/line 0 0 0 0 0 1000)
  
  (doseq [[i] (map list (range(count particleData)))]
    (q/sphere-detail 15)
    (q/with-translation ((particleData i) :coord)
      (q/sphere ((particleData i) :mass))))
  
  (updatePartData)
  )
;---------------------------------------------------------------------------------------------
(defn dataSetup [x]
  (def Data x)
  (q/defsketch quil-experiments
    :size [750 750]
    :setup setup
    :draw draw 
    :middleware [m/fun-mode]
    :renderer :opengl))
;---------------------------------------------------------------------------------------------
(defn LennardJonesPower [e q r]
  (if (> r 0)
    (if (<= r (* (Math/pow 2 (/ 1 6)) q))    (*(* e 0.02)-1)
      (if (< r (* 2.5 q))                    (*(/ (* 12 e) q)(-(Math/pow(/ q r) 13)(Math/pow (/ q r) 7)))  
        (* e 0.02)
        ))
    0)
  )
;---------------------------------------------------------------------------------------------
(defn Speed [t_step v_current m_ball LJpow]
  (+ v_current (* (/ LJpow m_ball) t_step))
  )
;---------------------------------------------------------------------------------------------
(defn newParticleCoordinate [t_step cord_current m_ball speed LJpow]
  (+ cord_current (* speed t_step) (* (/ LJpow m_ball) (/(Math/pow t_step 2)2)))
  )
;---------------------------------------------------------------------------------------------
(defn updatePartData [] 
  
  (doseq [[k] (map list (range(count particleData)))] 
    
    (let [[powLJ speed coord] [(into-array Double/TYPE [0]) (into-array Double/TYPE [((particleData k) :curSpeed)]) (to-array ((particleData k) :coord)) ]]
      
      (doseq [[j] (map list (range(count particleData)))]  
        (aset powLJ 0 (+ ((vec powLJ)0) (LennardJonesPower (Data 2) (Data 3) 
                                                           (Math/sqrt(+
                                                                       (Math/pow(-(((get (vec particleData) k):coord)0) (((get (vec particleData) j):coord)0))2)
                                                                       (Math/pow(-(((get (vec particleData) k):coord)1) (((get (vec particleData) j):coord)1))2)
                                                                       (Math/pow(-(((get (vec particleData) k):coord)2) (((get (vec particleData) j):coord)2))2)))
                                                           ))) 
        )  
      
      (aset speed 0 (Math/abs (Speed (Data 0) ((vec speed)0) ((particleData k) :mass) ((vec powLJ)0))))
      
      (doseq [[i] (map list (range(count coord)))]
        (aset coord i (newParticleCoordinate (Data 0) (((particleData k):coord)i) ((particleData i) :mass) ((vec speed)0) ((vec powLJ)0)))
        )      
      
      (def particleData (assoc particleData 0 1))
      
      ;        ;      (def particleDataCopy (assoc (particleData i) 
      ;        ;                                   :curSpeed (Math/abs (Speed (Data 0) ((particleData i) :curSpeed)((particleData i) :mass) ((vec buf)0)))
      ;        ;                                   :coord  [(newParticleCoordinate (Data 0) (((particleData i):coord)0) ((particleData i) :mass) ((particleData i) :curSpeed) ((vec buf)0))
      ;        ;                                            (newParticleCoordinate (Data 0) (((particleData i):coord)1) ((particleData i) :mass) ((particleData i) :curSpeed) ((vec buf)0))
      ;        ;                                            (newParticleCoordinate (Data 0) (((particleData i):coord)2) ((particleData i) :mass) ((particleData i) :curSpeed) ((vec buf)0))]
      ;        ;                                   ))      
      ;        ;          
      )
    )
  )
;---------------------------------------------------------------------------------------------
(defn -main []
  (dataSetup [10 0 100 65])
  )

;e-2 q-3
;(dataSetup [10 0 100 65])