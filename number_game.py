import random

# get a number to play with


def game():
  secret_num = random.randint(1,10)
  guesses = []
  maxg = 5
  print("It's the Number Game! You have {} guesses.".format(maxg))

  while len(guesses) < maxg:
      try:
          guess = int(input("Guess my number, it's an integer between 1 and 10: "))
      except ValueError:
          print("{} isn't an integer".format(guess))
      else:       
          if guess == secret_num:
              print("You guessed it! My number was {}".format(secret_num))
              break
          elif guess > secret_num:
              print("too high")
          elif guess < secret_num:
              print("too low")
          guesses.append(guess)
  else:
      print("FAIL")
  again = input("Do you want to play again? yes/no")
  if again == "yes":
      game()
  else:
      print("bye!")
game()
