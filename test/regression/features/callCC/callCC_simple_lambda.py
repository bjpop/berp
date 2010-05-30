print(callCC(lambda k: 56))
print(callCC(lambda k: k(12)))
print(callCC(lambda k: (k(8) + 42)))
