/*
** August 29, 2022
** Chiling Han
** 
** A user-interfaced program to return a Fibonacci sequence with a given number of values
** to be generated for ATCS:Expert Systems.
** 
** fibo      - returns the Fibonacci sequence with a given number of values to be generated
** isValid   - tests if input is a positive integer
** fibonacci - combines fibo and isValid
** fib       - prompts the user for the number of values to be generated of the Fibonacci sequence 
**
** To run the program, run Jess in terminal and then type (batch "./fibonacci.clp").
*/

(batch "./utilities_v4.clp") ; importing the utilities functions

/*
** This function returns a list of numbers of the Fibonacci sequence using a for-loop construct.
** It accepts an argument that is the number of values to be generated in this list.
** 
** Function assumes ?num is a positive integer
*/
(deffunction fibo (?num)
   (bind ?first 1l)  ; first term
   (bind ?second 1l) ; second term
   (bind ?list ())   ; list to return of the Fibonacci sequence
   
   (bind ?list (append$ ?list ?first))

   (if (> ?num 1l) then
      (bind ?list (append$ ?list ?second))
   )

   (for (bind ?i 2l) (< ?i ?num) (++ ?i)
      (bind ?curr (+ ?first ?second))
      (bind ?list (append$ ?list ?curr))

      (bind ?first ?second)
      (bind ?second ?curr)
   )

   (return ?list)

) ; deffunction fibo

/*
** This function validates the input. It returns TRUE if the input is a positive integer, FALSE otherwise.
**
*/
(deffunction isValid (?num)
   (return (and (numberp ?num) (or (not (floatp ?num)) (= ?num (long ?num))) (> ?num 0)))
)

/*
** This function combines isValid and fibo to return FALSE if isValid returns FALSE
** and generates a list of the Fibonacci sequence with a given number of values otherwise.
*/
(deffunction fibonacci (?num)
   (bind ?returnVal FALSE)

   (if (isValid ?num) then
      (if (not (longp ?num)) then
         (bind ?num (long ?num)) ; 
      )
      (bind ?returnVal (str-cat "The fibonacci sequence with " ?num " number(s) is: " (fibo ?num)))
   )

   (return ?returnVal)
) ; deffunction fibonacci

/*
** This function prompts the user for a number. Then, it will either print out the list of Fibonacci
** values or an error message if the input is invalid/not a positive integer.
*/
(deffunction fib ()
   (bind ?input (ask "Enter a number: "))

   (if (not (fibonacci ?input)) then
      (printline "Your input needs to be a positive integer.")
    else
      (printline (fibonacci ?input))
   )

   (return)
) ; deffunction fib
