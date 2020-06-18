;(test_sm Xvector Yvector MassVec)
(ns my-test1)
; Вычисление радиуса шарика из его массы и плотности
; Calculation of the ball radius from its mass and density
; m_ball - mass 
; p_ball - density
(defn BallRadiusCalculation [m_ball p_ball]
  (Math/pow
    (/(/ m_ball p_ball)(/ (* 4 (Math/PI)) 3))
    (/ 1 3))
  )
; Вычисление силы взаимодействия шариков используя потенциал Ленарда-Джонса
; Calculation of the force of interaction of balls using the Lenard-Jones potential
; e - the depth of the potential yawner
; r - the distance between the centers of the particles
; q - the distance at which the interaction energy becomes zero
(defn LennardJonesPower [e q r]
  (if (> r 0)
    (if (<= r (* (Math/pow 2 (/ 1 6)) q))
      -0.2
      (if (<= r (* 2.5 q)) 
        (-(* (/ (* 12 e) q)(-(Math/pow (/ q r) 13)(Math/pow (/ q r) 7)))
          (* (/ (* 12 e) q)(-(Math/pow (/ q (* 2.5 q)) 13)(Math/pow (/ q (* 2.5 q)) 7))))
        0    
        )
      )
    0
    )
  )
; Вычисление скорости
(defn Speed [t_step v_current m_ball LJpow]
  ( + v_current (* (/ LJpow m_ball) t_step))
  )
; Вычисление координаты Х 
(defn coordCulc [t_step x_current m_ball speed LJpow]
  (+
    (+ x_current (* speed t_step))
    (* (/ LJpow m_ball) (/(Math/pow t_step 2)2))
    )
  )
;
;;;
(defn newFN [inData]
  (def Data inData)
  (def particleData [
                     {
                      :coord[0 0 0]
                      :mass 5
                      :curLJPower 0
                      :curSpeed 0
                      }
                     
                     {
                      :coord[13 13 13]
                      :mass 5
                      :curLJPower 0
                      :curSpeed 0
                      }
                     
                     {
                      :coord[5 5 5]
                      :mass 5
                      :curLJPower 0
                      :curSpeed 0
                      }
                     
                     {
                      :coord[7 7 7]
                      :mass 5
                      :curLJPower 0
                      :curSpeed 0
                      }
                     ])
  
  (while (> (Data 4) 0)
    
    (doseq [[i] (map list (range(count particleData)))]
      (doseq [[j] (map list (range(count particleData)))]
        
     (def particleDataCopy (assoc (particleData i) :curLJPower (+ ((particleData i) :curLJPower) (LennardJonesPower (Data 2) (Data 3) 
                                                                                                                    (Math/sqrt(+
                                                                                                                                   (Math/pow(-(((get (vec particleData) i):coord)0) (((get (vec particleData) j):coord)0))2)
                                                                                                                                   (Math/pow(-(((get (vec particleData) i):coord)1) (((get (vec particleData) j):coord)1))2)
                                                                                                                                   (Math/pow(-(((get (vec particleData) i):coord)2) (((get (vec particleData) j):coord)2))2)))
                                                                                                                       ))))
     (def particleData (assoc particleData i particleDataCopy))
     )
      
   (def particleDataCopy (assoc (particleData i) :curSpeed (Speed (Data 0) ((particleData i) :curSpeed) 
                                                                     ((particleData i) :mass) ((particleData i) :curLJPower)
                                                                     
                                                                     
                                                                  )
                                :coord  [(coordCulc (Data 0) (((particleData i):coord)0) ((particleData i) :mass) ((particleData i) :curSpeed) ((particleData i) :curLJPower))
                                            (coordCulc (Data 0) (((particleData i):coord)1) ((particleData i) :mass) ((particleData i) :curSpeed) ((particleData i) :curLJPower))
                                            (coordCulc (Data 0) (((particleData i):coord)2) ((particleData i) :mass) ((particleData i) :curSpeed) ((particleData i) :curLJPower))]
                                ))      
      (def particleData (assoc particleData i particleDataCopy))
   )
      
  (doseq [[i] (map list (range(count particleData)))]   
      (def particleDataCopy (assoc (particleData i) :curLJPower 0 ))
      (def particleData (assoc particleData i particleDataCopy))
      )
  
    (def Data (assoc Data 4 (- (Data 4) (Data 0))))
    )
  (println particleData) 
  )
;        time_s0 v1 e2 q3 time_max4
;(newFN [0.1 0 1 10 1]) 