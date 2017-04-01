from functools import reduce

names = ['Mary', 'Isla', 'Sam']

hash_name = list(map(hash, names))

people = [{'name': 'Mary', 'height': 160},
          {'name': 'Isla', 'height': 80},
          {'name': 'Sam'}]

heights = list(map(lambda person: person['height'],
                   filter(lambda person: 'height' in person, people)))
mean = reduce(lambda a, h: a + h, heights) / len(heights)
