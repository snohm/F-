type  'a fbtree = Node of 'a * (('a fbtree) list)

let count = 