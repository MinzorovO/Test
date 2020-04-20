(ns my-test1)
(def Xvector [8 2 4 3 6])
(def Yvector [0 1 1 4 2])
(def MassVector [1 2 3 4 5])



;Реализация цикла for 
(defmacro for-loop [[sym init check change :as params] & steps]
  `(loop [~sym ~init value# nil]
     (if ~check
       (let [new-value# (do ~@steps)]
         (recur ~change new-value#))
       value#)))



;вычитание значений в скобках и возведение их в квадрат
(defn x1-x2Sqr[inpVector]
  
  (map
    (fn [x]
      (map
        (fn [y]
          (Math/pow (- x y) 2)
          )
        inpVector))
    inpVector)
  
  )



;Нахождение минимального расстояния между шариками
(defn DistanceBetweenBalls [inpVectorX inpVectorY i]  
  
  (Math/sqrt
    (apply min
           (findAndDelZero (vec (map + inpVectorX inpVectorY)))           
           )
    )
  
  )
;Нахождение и удаление "0"
(defn findAndDelZero [inpV]
  (remove-indexed
    inpV
    (first (indices zero? inpV))
    )
  )
;Нахождение индекса минимального значения
(defn findIndxOfMin [inpVec]
  (.indexOf 
    inpVec 
    (apply min 
           (findAndDelZero inpVec)
           )
    )
  )
;Нахождение индекса максимального значения
;(defn findIndxOfMax [inpVec]
;  (.indexOf 
;    inpVec 
;    (apply max 
;           (findAndDelZero inpVec)
;           )
;    )
;  )
;Нахождение индекса указанного значения
(defn indices [pred coll]
  (keep-indexed #(when (pred %2) %1) coll)
  )
;Удаление из массива по индексу
(defn remove-indexed [v n]
  (into (subvec v 0 n) (subvec v (inc n)))
  )



;временная функция для тестирования и отладки других функций
(defn test_sm [Xvector Yvector]
  
  (def tempVec (into-array (repeat (*(count Xvector)3) 0.)))
  
  
  (for-loop [i 0 (< i (count Xvector)) (inc i)] 
            
            (aset tempVec (* i 3) (double i))
            
            (aset tempVec (+(* i 3)1)
                  (double
                    (findIndxOfMin 
                      (vec (map + ((vec (x1-x2Sqr Xvector))i) ((vec (x1-x2Sqr Yvector))i)))
                      )
                    )
                  )
            
            (aset tempVec (+(* i 3)2)
                  (DistanceBetweenBalls 
                    ((vec (x1-x2Sqr Xvector))i) 
                    ((vec (x1-x2Sqr Yvector))i) 
                    i 
                    )
                  )     
            
            )
  
  (println
    (get 
      tempVec 
      (+(* (three (vec tempVec))3)2)
      )
    )
  
  
  (println
    (Xvector
      (int
        (get 
          tempVec 
          (+(* (three (vec tempVec))3)1)
          )
        )
      )
    )
  
  (println
    (Yvector
      (int
        (get 
          tempVec 
          (+(* (three (vec tempVec))3)1)
          )
        )
      )
    )
  
  )



;что-то наверное важное
(defn three [tempVec]
  (def tempVec_2 (into-array (repeat (/(count tempVec)3) 0.)))
  
  (for-loop [i 0 (< i (count Xvector)) (inc i)]
            
            (aset tempVec_2 i  
                  (double 
                    (get tempVec (+(* i 3)2))
                    )
                  )
            
            )
  
  (.indexOf 
    (vec tempVec_2)
    (apply min 
           (vec tempVec_2)
           )
    )
  
  )









