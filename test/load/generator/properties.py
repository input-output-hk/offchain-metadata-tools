from random import choice
from string import ascii_uppercase

properties = ["subject", "url", "name", "ticker", "policy", "logo", "description"]

def get_random_property():
    return choice(properties)

def get_invalid_property():
    return (''.join(choice(ascii_uppercase) for i in range(8)))

if __name__ == '__main__':
    pass