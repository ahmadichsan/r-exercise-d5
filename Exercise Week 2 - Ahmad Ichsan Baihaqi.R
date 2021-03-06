### Exercise Week 2

### Author: Ahmad Ichsan Baihaqi
### Email: ahmadichsanbaihaqi@gmail.com

# Question 1
questionOne = function() {
  # start sample data
  names = c('Andi', 'Budi', 'Charlie', 'Dilan', 'Echa')
  scores = c(100, 100, 80, 80, 85)
  # end of sample data
  
  # start create function
  thirdPlace = function(particNames, particScores) {
    data = data.frame(particNames, particScores)
    orderedData = data[order(-particScores), ]
    
    uniqueScores = unique(orderedData$particScores)
    totalUniqueScores = length(uniqueScores)
    
    message = "The third winner:"
    
    if (totalUniqueScores <= 2) {
      return (paste(message, "No third winner"))
    }
    
    thirdScore = uniqueScores[3]
    thirdScoreRequirement = orderedData$particScores == thirdScore
    
    thirdPosition = orderedData[thirdScoreRequirement,]
    thirdPositionConcat = paste(thirdPosition$particNames, collapse = ", ")
    
    return (paste(message, thirdPositionConcat))
  }
  # end of create function
  
  # main
  firstQuestionAnswer = thirdPlace(names, scores)
  print(firstQuestionAnswer)
}

questionOne()

# Question 2
# reference:
# https://dk81.github.io/dkmathstats_site/rmath-quad-formula-r.html
# http://jwilson.coe.uga.edu/EMAT6680Su10/Spooner/Assignment2KS/Assignment2KS.html
# https://courses.lumenlearning.com/boundless-algebra/chapter/introduction-to-quadratic-functions/
questionTwo = function() {
  squareRoot = function(a, b, c) {
    # a quadratic equation should follow this rule
    # 1. The quadratic equation must equal zero, ax\^2 + bx + c = 0
    # 2. a must not equal zero
    
    # for this case, assumed that the rule number 1 always fulfill. So, we have to handle rule number 2.

    if (a == 0) {
      return ('Oops, this is not a quadratic equation, but a linear equation. So, No Solution.')
    }
    
    # eq to find roots of a quadratic formula is as follow:
    # x = (-b ± √b^2 - 4*a*c) / (2a*a)
    # the part √b^2 - 4*a*c is called a discriminant.
    # discriminant has three cases:
    # a. if discriminant > zero. This means there would be 2 distinct solutions for x (or x-intercepts).
    # b. if discriminant = zero. This means there would be one value for x.
    # c. if discriminant < zero. This means the square root of a negative value is undefined.
    # There would be no real-numbered values for x.
    
    discriminant = (b^2) - (4*a*c)
    
    if (discriminant < 0) {
      return ('No Solution')
    }
    
    if (discriminant == 0) {
      x = (-b) / (2*a)
      return (paste("X =", x))
    }
    
    # case if discriminant > 0
    positiveX = (-b + sqrt(discriminant)) / (2*a)
    negativeX = (-b - sqrt(discriminant)) / (2*a)
    
    return (paste0("X1 = ", positiveX, ", ", "X2 = ", negativeX))
  }
  
  sample1 = squareRoot(1, -5, 6) # Output: 'X1 = 3, X2 = 2'.
  print(sample1)
  
  sample2 = squareRoot(2, 4, 2) # Output; 'X = -1'.
  print(sample2)
  
  sample3 = squareRoot(1, 1, 9) # Output: 'No solution'.
  print(sample3)
  
  sample4 = squareRoot(0, 1, 2) # Output: 'Oops, this is not a quadratic equation, but a linear equation. So, No Solution.'.
  print(sample4)
}

questionTwo()

# Question 3
questionThree = function() {
  distance = function(point1, point2) {
    q1 = point1[1]
    q2 = point1[2]
    
    p1 = point2[1]
    p2 = point2[2]
    
    result = sqrt((q1 - p1)^2 + (q2 - p2)^2)

    return (paste("The distance is", result))
  }
  
  sample1 = distance(c(5, 5), c(1, 2)) # Output: "The distance is 5"
  print(sample1)
  
  sample2 = distance(c(0, -1), c(-3, 2)) # Output: "The distance is 4.242640487"
  print(sample2)
}

questionThree()
