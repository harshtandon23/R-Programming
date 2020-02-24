# Name: Harsh Tandon
# Email: htandon@scu.edu
# Student ID: W1580393
# Date: 10/07/2019


####function to generate Fibonacci Series
generate_Fibonacci = function(x){
   FibSeq = c(0,1)                                  #vector containing first two elements of Fibonacci Series
   count = 2                                        #count variable to track #elements in the series
   if(x == 1){                                      #print only first term if requested series is of length 1
     cat("Fibonacci Sequence: ", FibSeq[1], "\n")   
   }
   else{                                            
     i = 1                                          #index variable to access elements in vector FibSeq
     while(count < x){
       nextVal = FibSeq[i]+FibSeq[i+1]              #calculate the next term of series by adding previous two terms
       FibSeq = append(FibSeq, nextVal, after = i+1)#append the new term to sequence on the right
       i = i+1                                      #increment index variable
       count = count+1                              #increment count variable
     }
     cat("Fibonacci Sequence: ", FibSeq, "\n")      #print the series using cat() function
   }
}



####function to generate FizzBuzz 
FizzBuzz = function(x){
  i = 1                             #counter that increments from 1 to x 
  while(i <= x){                    #while i is less than x, do the following
    if(i%%15 == 0)                  #if i is a multiple of 3 and 5 both, then print FizzBuzz
      cat(i," FizzBuzz\n")
    else if(i%%3 == 0)              #if i is a multiple of 3 only, then print Fizz
      cat(i," Fizz\n")
    else if(i%%5 == 0)              #if i is a multiple of 5 only, then print Buzz
      cat(i," Buzz\n")
    else                            #if i is neither a multiple of 3 nor 5, then just print the number
      cat(i,"\n")
    i = i+1                         #increment the counter
  }                                 #while loop closes
}



####function to check if the entered number x is prime
isPrime = function(x){
  i = 2
  while(i<=sqrt(x)){                #while divisor (i) is less than square root of number(x)
    if(x%%i == 0)                   #if divisor (i) is a multiple of x, then x is not a prime number. Return False
      return(FALSE)
    i = i+1                         #increment the divisor
  }
  return(TRUE)                      #if the loop ends without returning False that means x is Prime. Return True
}



####function to generate Prime Numbers till a limit x
generate_PrimeSeq = function(x){
  i = 2                             #counter that increments from 2 to x
  index = 0                         #index variable to access PSeq vector 
  PSeq = c()                        #empty vector that will contain prime numbers (if any) between 2 and x
  while(i <= x){                    #loop to check each number from 2 to x and add in a vector if it is prime
    if(isPrime(i)){                 #function call to isPrime(x) function to check if i is a prime number
      index = index+1
      PSeq = append(PSeq, i, index) #append the prime number to the right of the vector
    }
    i = i+1
  }
  if(length(PSeq!=0))               #if any prime numbers are found, print them. Else print Not Found message
    cat("Sequence of prime numbers till",x,"is : ",PSeq,"\n")
  else
    cat("There are no prime numbers\n")
}



####function to generate random vector of a given size x 
generate_RandomVector = function(x){
  rvect = sample(-50:50, x, replace = TRUE)           #sample() to create random numbers vector of
                                                      #size x between -50 & 50
  cat("Random vector of size",x,"is: ",rvect,"\n")
}


########################################################


####Main Function to print menu and accept user input

LoopCondition = TRUE                                    #Condition Variable to run the loop

while(LoopCondition){                                   #loop until Q or q is entered by user
  cat("Please select the function you want to perform:
      Press A - Generate Fibonacci Numbers
      Press B - FizzBuzz
      Press C - Generate Prime Numbers
      Press D - Create Random Vector
      Press Q - Quit")
  n = readline(prompt = "Enter your choice : ")         #read user input to select the function from the menu
  
  if (n == "Q" || n == "q"){                            #if Quit command is selected by user, break the loop
      print("Thank you for using the program!")
      LoopCondition = FALSE
      break
  } else if (n == "A" || n == "a"){                     #if Fibonacci is selected by user, prompt an integer input
      x = as.integer(readline(prompt = "Enter number of terms required: "))
      if (is.na(x))                                     #proceed further only if the integer input is valid
        print("Wrong Input! Please enter a number")
      else if(x <= 0)                                   #proceed further only if the input is greater than 0
        print("Please enter a number greater than 0")
      else                                              #if input (x) is valid, call generate_Fibonacci(x)
        generate_Fibonacci(x)
  } else if (n == "B" || n == "b"){                     #if FizzBuzz is selected by user, prompt an integer input
      x = as.integer(readline(prompt = "Enter a number: "))
      if (is.na(x))
        print("Wrong Input! Please enter a number")
      else if(x <= 0)                                   
        print("Please enter a number greater than 0")
      else                                              #if input (x) is valid, call FizzBuzz(x)
        FizzBuzz(x)
  } else if (n == "C" || n == "c"){                     #if Prime is selected by user, prompt an integer input
      x = as.integer(readline(prompt = "Enter a number: "))
      if (is.na(x))
        print("Wrong Input! Please enter a number")
      else if(x <= 0)
        print("Please enter a number greater than 0")
      else                                              #if input (x) is valid, call generate_PrimeSeq(x)
        generate_PrimeSeq(x)
  } else if (n == "D" || n == "d"){                     #if Random Vector is selected by user, prompt an integer input
      x = as.integer(readline(prompt = "Enter length of vector: "))
      if (is.na(x))
        print("Wrong Input! Please enter a number")
      else if(x < 0)
        print("Please enter a positive number")
      else                                              #if input (x) is valid, call generate_RandomVector(x)
        generate_RandomVector(x)
  } else
      print("Wrong Input! Please try again")
}
