# Open a file containing points and another their class, and output a geojson file containing these information

import json, sys, random

data = {
    "type": "FeatureCollection",
    "features": []
}

if len(sys.argv) != 4:
    print("Usage: python3 classes.py <input_points> <input_classes> <output_file>")
    sys.exit(1)

input_points = sys.argv[1]
input_classes = sys.argv[2]
output_file = sys.argv[3]

points = open(input_points, "r")
classes = open(input_classes, "r")
colors = []

nb_points = int(points.readline())
nb_classes = int(classes.readline())

for i in range(nb_classes):
    colors.append(random.randint(0, 16777215))

for i in range(nb_points):
    point = points.readline().split(',')
    cl = int(classes.readline())
    data["features"].append({
        "type": "Feature",
        "geometry": {
            "type": "Point",
            "coordinates": [float(point[0]) * 10 + 800000, float(point[1]) * 10 + 6000000]
        },
        "properties": {
            "color": '#'+format(colors[cl], '06x')
        }
    })

with open(output_file, "w") as output:
    json.dump(data, output)