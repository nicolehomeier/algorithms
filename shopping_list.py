shopping_list = []
def show_help():
  print("What do you need to pick up? ")
  print("""
Enter 'Done' to stop adding items.
Enter 'Help' for this help.
Enter 'Show' to see your current list.""")

  
def show_list():        
  print("List:")
  for item in shopping_list:
      print(item)

def add_to_list(new_item):
  shopping_list.append(new_item)
  print("Added {} to your list.".format(new_item))
  
show_help()

while True:
    new_item = input("> ")
    if new_item == "Done":
        break
    elif new_item == "Help":
        show_help()
        continue
    elif new_item == "Show":
        show_list()
        continue
    add_to_list(new_item)   
        
