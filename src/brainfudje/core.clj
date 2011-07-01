(ns brainfudje.core)

(defn find-char [input curr cha func]
  "Reads through input, returning the index 1 beyond the nearest character that matches cha, iterating by func"
  (cond
   (>= curr (count input)) (count input) 
   (< curr 0) 0                          
   (= (nth input curr) cha) (+ curr 1)   ; if found, return the index immediately after cha
   :else (recur input (func curr) cha func)))

(defn bf-interp [input instr d-array ptr]
  "brainfudge's interpreter, keeping track of input, current instruction point,
  data-array, and data pointer"
  (let [top (nth input instr -1)
        cell (nth d-array ptr)]
    (cond
     (= -1 top) (list d-array ptr) ; if the instruction pointer is out-of-bounds return
     (= top \>) (cond
                 (= ptr (- (count d-array) 1)) (recur input (+ instr 1) d-array ptr)
                 :else (recur input (+ instr 1) d-array (+ ptr 1)))
     (= top \<) (cond
                 (= ptr 0) (recur input (+ instr 1) d-array ptr)
                 :else (recur input (+ instr 1) d-array (- ptr 1)))
       (= top \+) (recur input (+ instr 1) (assoc d-array ptr (mod (+ cell 1) 256)) ptr)
       (= top \-) (recur input (+ instr 1) (assoc d-array ptr (mod (- cell 1) 256)) ptr)
       (= top \,) (let [in (int (first (read-line)))]
                    (flush)
                    (recur input (+ instr 1) (assoc d-array ptr (mod (+ cell in) 256)) ptr))
       (= top \[) (if (= 0 (nth d-array ptr)) (recur input (find-char input instr \] inc) d-array ptr)
                      (recur input (+ instr 1) d-array ptr))
       (= top \]) (if  (not= 0 (nth d-array ptr)) (recur input (find-char input instr \[ dec) d-array ptr)
                       (recur input (+ instr 1) d-array ptr))
       (= top  \.) (do
                     (println (char (nth d-array ptr)))
                     (flush)
                     (recur input (+ instr 1) d-array ptr))
       :else (recur input (+ instr 1) d-array ptr))))

(defn bf-repl [d-array ptr]
  "brainfudge repl, continually updates the d-array and pointer to current cell"
  (do
    (print "brainfudje> ")
    (flush)
    (let [input (read-line)
          out-tup (bf-interp input 0 d-array ptr)]
      (recur (first out-tup) (second out-tup)))))

(bf-repl (vec (take 30000 (repeat 0))) 0)