import random

# make a list of words
words = [
  'carrot',
  'eggplant',
  'broccoli',
  'lettuce',
  'turnip',
  'asparagus',
  'radish'
]

while True:
    start = input("Press enter to start, or enter Q to quit")
    if start.lower() == 'q':
        break

# pick a random word
    target_word = random.choice(words)
    wrong = []
    right = []
  # draw spaces
    while len(wrong) < 7 and len(right) != len(list(target_word)):
        for letter in target_word:
            if letter in right:
                print(letter,end='')
            else:
                print('_',end='')
        print('')
        print('Strikes: {}/7'.format(len(wrong)))
        print('')
              
  # take guesses
        guess = input("Guess a letter: ").lower()
    
        if len(guess) != 1:
            print("Only guess a single letter! ")
            continue
        elif guess in wrong or guess in right:
            print("You've already guessed that letter!")
            continue
        elif not guess.isalpha():
            print("Only guess letters!")
            continue
        elif guess in target_word:
            right.append(guess)
            if len(right) == len(list(target_word)):
                print("You win! The word was {}".format(target_word))
                break
        else:
            wrong.append(guess)
    else:
        print("You lose! The word was {}.".format(target_word))
    # draw guessed letters and strikes
    # print out win/lost
    
    
