import shelve

def enter_command():
    cmd = input("Enter command(? for help):")
    cmd = cmd.strip().lower()
    return cmd

def store_person(db):
    id = input("Enter ID:")
    person = {}
    person['name'] = input('Enter name:')
    person['age'] = input('Enter age:')
    db[id] = person

def lookup_person(db):
    id = input("Enter ID:")
    print(db[id])

def print_help():
    print("""The available commands are:
    store: stores information
    lookup: looks up a person by ID
    quit: save change and exit
    ?: prints this message
    """)

def main():
    database = shelve.open("./database.dat")
    try:
        while True:
            cmd = enter_command()
            if cmd == 'store':
                store_person(database)
            elif cmd == 'lookup':
                lookup_person(database)
            elif cmd == '?':
                print_help()
            elif cmd == 'quit':
                return 
    finally:
        database.close()

if __name__ == '__main__': main()


# 以下是运行结果

# ➜  Python git:(master) ✗ python3 database.py
# Enter command(? for help):'?
# The available commands are:
#     store: stores information
#     lookup: looks up a person by ID
#     quit: save change and exit
#     ?: prints this message
#     
# Enter command(? for help):'store
# Enter ID:1
# Enter name:Lee
# Enter age:10
# Enter command(? for help):'store
# Enter ID:2
# Enter name:Chen
# Enter age:12
# Enter command(? for help):'quit
# ➜  Python git:(master) ✗ python3 database.py
# Enter command(? for help):lookup
# Enter ID:1
# {'name': 'Lee', 'age': '10'}
# Enter command(? for help):lookup
# Enter ID:2
# {'name': 'Chen', 'age': '12'}
# Enter command(? for help):quit