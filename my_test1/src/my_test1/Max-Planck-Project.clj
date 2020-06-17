;(test_sm Xvector Yvector MassVec)
(ns my-test1)
;начало блока удаления данных из массива 
(defn index-exclude [r ex] 
  "Take all indices execpted ex" 
  (filter #(not (ex %)) (range r))) 
(defn dissoc-idx [v & ds]
  (map v (index-exclude (count v) (into #{} ds))))
; конец блока
;Нахождение индекса указанного значения
(defn indices [pred coll]
  (keep-indexed #(when (pred %2) %1) coll)
  )
;Нахождение и удаление первого "0"
(defn findAndDelZero [inpVec]
  (dissoc-idx
    inpVec
    (first (indices zero? inpVec))
    )
  )
;Нахождение индекса минимального значения
(defn findIndxOfMin [inpVec]
  (.indexOf 
    inpVec 
    (apply min (findAndDelZero inpVec))
    )
  )
;вычитание значений в скобках и возведение их содержимого в квадрат
(defn x1-x2Sqr[inpVec]
  (map (fn [x] (map (fn [y] (Math/pow (- x y) 2)) inpVec))inpVec)
  )
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
      -5
      (if (<= r (* 2.5 q)) 
        (-(* (/ (* 12 e) q)(-(Math/pow (/ q r) 13)(Math/pow (/ q r) 7)))
          (* (/ (* 12 e) q)(-(Math/pow (/ q (* 2.5 q)) 13)(Math/pow (/ q (* 2.5 q)) 7))))
        0    
        )
      )
    0
    )
  )

; Вычисление ускорения учитывая силу взаимодействия и массы шарика
; Calculation of speed-up given the strength of the interaction and the mass of the ball
(defn Speed_up [m_ball LJpow]
  (/ LJpow m_ball)
  )

; Вычисление скорости
(defn Speed [t_step v_current m_ball LJpow]
  ( + v_current (* (Speed_up m_ball LJpow) t_step))
  )

; Вычисление координаты Х 
(defn coordCulc [t_step x_current m_ball speed LJpow]
  (+
    (+ x_current (* speed t_step))
    (* (Speed_up m_ball LJpow) (/(Math/pow t_step 2)2))
    )
  )
;
;;;
(defn newFN [inData]
  
  (def Data inData)
  
  (def particleData [
                     {
                      :coord[1 1 1]
                      :mass 5
                      :curLJPower 0
                      :curSpeed 0
                      :tempDist [10 10 10 10]
                      }
                     
                     {
                      :coord[13 13 13]
                      :mass 5
                      :curLJPower 0
                      :curSpeed 0
                      :tempDist [0 0 0 0]
                      }
                     
                     {
                      :coord[5 5 5]
                      :mass 5
                      :curLJPower 0
                      :curSpeed 0
                      :tempDist [0 0 0 0]
                      }
                     
                     {
                      :coord[7 7 7]
                      :mass 5
                      :curLJPower 0
                      :curSpeed 0
                      :tempDist [0 0 0 0]
                      }
                     ])
  
  ;(println "count" (count particleData))
  (println "CHENGED")
  
  (while (> (Data 4) 0)
    
    (doseq [[i] (map list (range(count particleData)))]
      (doseq [[j] (map list (range(count particleData)))]
        
        (println i j)
        
        (def particleDataCopy (assoc (particleData i) :curLJPower (+ ((particleData i) :curLJPower) 0 (LennardJonesPower (Data 2) (Data 3) 
                                                                                                                         (Math/sqrt(+
                                                                                                                                     (Math/pow(-(((get (vec particleData) i):coord)0) (((get (vec particleData) j):coord)0))2)
                                                                                                                                     (Math/pow(-(((get (vec particleData) i):coord)1) (((get (vec particleData) j):coord)1))2)
                                                                                                                                     (Math/pow(-(((get (vec particleData) i):coord)2) (((get (vec particleData) j):coord)2))2)))
                                                                                                                         
                                                                                                                         )
                                                                     )))
        (def particleData (assoc particleData i particleDataCopy))
        
        )
      (println i ((particleData i) :curLJPower))
      
      
      )
    
    (doseq [[i] (map list (range(count particleData)))]   
      (def particleDataCopy (assoc (particleData i) :curLJPower 0 ))
      (def particleData (assoc particleData i particleDataCopy))
      )
    
    (println Data)
    (def Data (assoc Data 4 (- (Data 4) (Data 0))))
    )
  )








;        (println 
;          (LennardJonesPower
;            (Data 2)
;(Math/sqrt(+
;            (Math/pow(-(((get (vec particleData) i):coord)0) (((get (vec particleData) j):coord)0))2)
;            (Math/pow(-(((get (vec particleData) i):coord)1) (((get (vec particleData) j):coord)1))2)
;            (Math/pow(-(((get (vec particleData) i):coord)2) (((get (vec particleData) j):coord)2))2)))
;            (Data 3)
;            )
;          )







;  (def particleData {:coordX [13 20 15 18]
;                     :coordY [0 12 7 4]
;                     :coordZ [4 8 17 1]
;                     :massPart [5 10 15 20]
;                     :tempDist [999 999 999]
;                     :realDist [999 999 999 999]
;                     :vCur [0 0 0 0]
;                     })
;
;(while (>(get inData 4) (get inData 0))
;    
;    
;    (doseq [[x y z i] (map list (x1-x2Sqr(vec(particleData :coordX))) 
;                           (x1-x2Sqr(vec(particleData :coordY)))      
;                           (x1-x2Sqr(vec(particleData :coordZ)))      
;                           (range (count(particleData :coordX))))]    
;      
;      (doseq [[k] (map list (range(count(findAndDelZero (vec z)))))]
;        (def particleData (assoc particleData   
;                                 :tempDist  (assoc (particleData :tempDist) k  
;                                                   
;                                                   (Math/sqrt (+
;                                                                (get (vec(findAndDelZero (vec x))) k)
;                                                                (get (vec(findAndDelZero (vec y))) k)
;                                                                (get (vec(findAndDelZero (vec z))) k)))
;                                                   )
;                                 )))
;      
;      (if (>
;            (apply min (particleData :tempDist))
;            (* (Math/pow 2 (/ 1 6)) (get inData 3))
;            )
;        
;        (def particleData (assoc particleData 
;                                 :coordX  (assoc (particleData :coordX) i (coordCulc  
;                                                                            (get inData 0) (get (particleData :coordX) i) (get (particleData :massPart) i) 
;                                                                            (get(particleData :vCur)i) (get inData 2) (apply min (particleData :tempDist)) (get inData 3)
;                                                                            ))
;                                 :coordY (assoc (particleData :coordY) i (coordCulc  
;                                                                           (get inData 0) (get (particleData :coordY) i) (get (particleData :massPart) i) 
;                                                                           (get(particleData :vCur)i) (get inData 2) (apply min (particleData :tempDist)) (get inData 3)
;                                                                           ))
;                                 :coordZ (assoc (particleData :coordZ) i (coordCulc  
;                                                                           (get inData 0) (get (particleData :coordZ) i) (get (particleData :massPart) i) 
;                                                                           (get(particleData :vCur)i) (get inData 2) (apply min (particleData :tempDist)) (get inData 3)
;                                                                           ))
;                                 ))
;        )
;      
;      (def particleData (assoc particleData 
;                               :realDist  (assoc (particleData :realDist) i  
;                                                (Math/sqrt (+
;                                                             (get (particleData :coordX) i)
;                                                             (get (particleData :coordY) i)
;                                                             (get (particleData :coordZ) i)
;                                                             ))
;                                                )
;                               :vCur (assoc (particleData :vCur) i (Speed (get inData 0) (get(particleData :vCur)i) (get (particleData :massPart) i) 
;                                                                          (get inData 2) (apply min (particleData :tempDist)) (get inData 3)))
;                               ))
;      )
;    )








;        time_s0 v1 e2 q3 time_max4
;11 -25
;(newFN [0.1 0 1 10 1]) 