/*
** September 19, 2022
** Chiling Han
**
** A user-interfaced program to dynamically assert letters in the knowledge base
** and create anagrams from those letters for ATCS:Expert Systems.
**
** promptUser   - a rule to run the anagram program
** assertLetter - asserts a Letter fact in the knowledge base
** charsToFact  - makes a fact for each character in a list of characters
** slice$       - returns a list of characters from a string
** isValid      - tests if input is a string with less than eleven letters
** assembleRule - creates a string for dynamic rule creation
** anagram      - prompts the user to dynamically assert the inputted characters and create anagrams
**
** To run the program, run Jess in terminal and then type (batch "./anagram.clp").
*/

(clear)
(reset)

;;;;;;;;;;;;

(batch "./utilities_v4.clp")
                                  
(defglobal ?*MAX_LETTERS* = 10) ; constant for the maximum number of letters in the user's inputted word
                                ; used to validate the user's input

/*
** Defines a template of a letter with a character and its identifier for a word.
** 
** slot c is the character in the word
** slot p is the unique identifier of the character in the word
*/
(deftemplate Letter (slot c) (slot p))

(defrule promptUser
   "This rule has no LHS so it will always fire. It will ask the user for a word by calling anagram."
   (declare (salience 1))
   =>
   (anagram)
)

/*
** Using the Letter template, this function asserts a Letter fact when provided
** with a character and the unique identifier of that character in the word.
**
** Function assumes that ?char is a character and ?pos is an integer between 1 and the number of
** characters in the word.
**
*/
(deffunction assertLetter (?char ?pos)
   (assert (Letter (c ?char) (p ?pos)))

   (return)
)

/*
** This function takes in a list of characters and makes a fact for each character
** in the list by calling the assertLetter function.
**
** Function assumes that ?l is a list of characters.
*/
(deffunction charsToFact (?l)
   (for (bind ?i 1) (<= ?i (length$ ?l)) (++ ?i)
      (bind ?c (nth$ ?i ?l))
      (assertLetter ?c ?i)
   )

   (return)
)

/*
** This function explodes and returns a list of characters given a string.
** 
*/
(deffunction slice$ (?string)
   (bind ?list ())

   (for (bind ?i 1) (<= ?i (str-length ?string)) (++ ?i)
      (bind ?char (sub-string ?i ?i ?string))
      (bind ?list (append$ ?list ?char))
   )

   (return ?list)
) ; deffunction slice$

/*
** This function validates the input. It returns TRUE if the input is a list of alphabetical characters
** and has less than ten letters, FALSE otherwise.
**
** Function takes in a list of characters.
*/
(deffunction isValid (?input)
   (bind ?ASCII_UPPER_A 65) ; constant for ascii code value of character "A" is 65
                            ; used to check if the input is alphabetical  

   (bind ?ASCII_UPPER_Z 90) ; constant for ascii code value of character "Z" is 90
                            ; used to check if the input is alphabetical                       
   
   (bind ?isAlpha TRUE)
   (foreach ?c ?input
      (bind ?c (upcase ?c))
      (if (or (< (asc ?c) ?ASCII_UPPER_A) (> (asc ?c) ?ASCII_UPPER_Z)) then
         (bind ?isAlpha FALSE)
      )
   )
                           
   (return (and ?isAlpha (<= (length$ ?input) ?*MAX_LETTERS*)))
) ; deffunction isValid

/*
** This function creates a rule to create anagrams of a given word and returns the string to build for
** dynamic rule creation.
**
** Function takes in the number of letters in the word.
*/
(deffunction assembleRule (?letters)
   (bind ?ruleString "(defrule rule-1 ")

   (for (bind ?i 1) (<= ?i ?letters) (++ ?i)
      (bind ?ruleString (str-cat ?ruleString (str-cat "(Letter (c ?c" ?i ") (p ?p" ?i)))

      (for (bind ?j (- ?i 1)) (>= ?j 1) (-- ?j)
         (bind ?ruleString (str-cat ?ruleString (str-cat " &~?p" ?j)))
      )

      (bind ?ruleString (str-cat ?ruleString ")) "))
   ) ; (for (bind ?i 1) (<= ?i ?letters) (++ ?i)

   (bind ?ruleString (str-cat ?ruleString " => (printout t "))

   (for (bind ?i 1) (<= ?i ?letters) (++ ?i)
      (bind ?ruleString (str-cat ?ruleString "?c" ?i " "))
   )

   (bind ?ruleString (str-cat ?ruleString "crlf))"))

   (return ?ruleString)
) ; deffunction assembleRule


/*
** This function prompts the user for a word of arbitrary length. Then, it will dynamically build a rule
** to print the anagrams of the given word.
*/
(deffunction anagram ()
   (bind ?string (askline "Enter a word: "))
   (bind ?charList (slice$ ?string))
   
   (if (isValid ?charList) then
      (build (assembleRule (length$ ?charList)))
      (charsToFact ?charList)

    else
      (printline (str-cat "Your input has to be a word with no more than " ?*MAX_LETTERS* " letters."))
   )

   (return)
) ; deffunction anagram

(run)
