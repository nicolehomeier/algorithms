def loopy(items):
    # Code goes here
    for item in items:
        temp = list(item)
        if temp[0] == "a":
          continue
        else:
            print(temp)
loopy(["abc","xyz"])
