/*
** August 25, 2022
** Chiling Han
** 
** A user-interfaced program to find the factorial of a given number for ATCS:Expert Systems.
** 
** fact - returns the factorial of a number
** factorial - validates user input and returns factorial of the number
**
** To run the program, run Jess in terminal and then type (batch "./factorial.clp").
*/

(batch "./utilities_v4.clp") ; import the utilities functions

/*
** This function returns the factorial of a given number.
** 
** Function assumes iVal is a nonnegative integer
*/
(deffunction fact (?iVal)
   (if (= ?iVal 0) then
      (return 1)
    else (return (* ?iVal (fact (- ?iVal 1))))
   )
)

/*
** This function asks the user for a number to find its factorial.
** It uses the ask() utilities function and calls the fact() function.
** 
** Function validates if the user input is a nonnegative integer; otherwise, it produces
** a descriptive error message.
*/
(deffunction factorial ()
   (bind ?num (ask "Enter a number: "))

   (if (numberp ?num) then
      (if (or (not (floatp ?num)) (= ?num (long ?num))) then

         (if (not (longp ?num)) then
            (bind ?num (long ?num))
         )

         (if (>= ?num 0) then
            (printline (str-cat "The factorial of " ?num " is " (fact ?num)))
          else
            (printline "Your input has to be a nonnegative.")
         )

       else (printline "Your input has to be an integer.")
      ) ; if (or (not (floatp ?num))...)

    else (printline "Your input has to be a number.")
   ) ; if (numberp ?num)

   (return)
) ; deffunction factorial
