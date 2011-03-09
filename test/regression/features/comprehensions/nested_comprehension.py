emptyListComp = [x for x in []]
singleListComp = [x for x in [1]]

emptySetComp = {x for x in []}
singleSetComp = {x for x in [1]}

# list nested with list
print([x for x in emptyListComp])
print([x for x in singleListComp])

# set nested with set
print({x for x in emptySetComp})
print({x for x in singleSetComp})

# list nested with set
print([x for x in emptySetComp])
print([x for x in singleSetComp])
