<div align="center">
    <img src="assets/illustration.png" alt="illustration" width="160"/>
    <h1>TIPE 2023 - The city</h1>
    <h3>Tracing of paths for reconnaissance of an earthquake zone by drone</h3>
</div>

This is my final project for the french preparatory classes (CPGE) competitive exams. This year's theme was "The city", so I decided to create a tool to create trajectories for drones that would be deployed during earthquakes, in order to split work of emergency services between zones that were identified according to the topography (population density, types of infrastructures...). 

All resources and materials used are referenced on the [Notion page](https://theo-vidal.notion.site/TIPE-2023-La-ville-e76f332f69aa47368b4cce6426bfec1d?pvs=74) of the project (in French), as well as some general considerations and explanations.

## 💻 Develop

This project is programmed in OCaml and uses opam and dune for building.

Build all parts:

```bash
dune build
```

You can also replace the executable file by `dune exec ...` in the commands below to directly use OCaml's interpeter.

Output is located in the _build directory. You can then execute the different parts:

### Topography tidy-up

This sub-program takes as input a list of points representing topographic features, filters them by proximity regarding their distance, and calculates an associated weight.

To get a list of points from a map, you can use [QGIS](https://qgis.org/) for instance. In the context of this project, I recommend calculating building's centroid as points.

Run the program:

```bash
topography [input] [THRESHOLD] (output=selection.csv)
```

Arguments:
- `input`: path to a .csv file containing following columns: `x,y,nature,use,state,nb_housing,height`:
    - `x` and `y` coordinates of the point
    - `nature`: the type of the building (generally tower, agriculture or the rest)
    - `use`: its current use (residential, commercial, ...)
    - `state`: in use, not used, abandonned...
    - `nb_housing`: if residential, the number of housing contained in the building
    - `height`: height of the building, in meters
- `threshold`: the number of points in the output selection
- `output` (default: _selection.csv_): a .csv file containing following columns: `x,y,weight`

This program can be configured by adding a `settings.json` file in the same directory, in order to adjust the weight applied to each building type depending on relevant parameters:

- Dependance on `use`:
    - Residential: Total weight = Residential weight + Housing weight * Number of housing
    - Commercial or industrial: Total weight = Residential weight
- Dependance on `nature`: when the building is a tower, its height is modified by taking the square, to account for debris dispersion during an earthquake

The default configuration is:

```json
{
    "default_weight": 1.0,

    "residential_label": "Résidentiel",
    "residential_weight": 1.0,
    "residential_housing_weight": 0.1,

    "commercial_label": "Commercial et services",
    "commercial_weight": 1.5,

    "industrial_label": "Industriel",
    "industrial_weight": 1.8,

    "health_label": "Santé",
    "health_weight": 2.0,

    "tower_label": "Tour et donjon",
    "tower_weight": 2.0
}
```

### Zones

This program divides points into small sub-groups, which size and shape is determined by the chosen method.

Run the program:

```bash
zones (input) [NB_ZONES] [ALPHA] [NB_ROLLS] (method) (output=classes.txt)
```

Arguments:
- `input`: path to a `.csv` file containing following columns: `x,y,weight`
- `NB_ZONES`: integer, number of zones wanted
- `NB_ROLLS`: integer, number of loop iterations in the chosen algorithm
    - Only applied when method is kmeans or geometric
- `method`: the chosen method to subdivise the ensemble
    - Options available: `kmeans`, `kcenters`, `geometric` (_Geometric mesh partition_ ; see [references](#-references))
- `output` (default: _classes.txt_): path to the output file, where each line contains an integer, corresponding to the class number of the point located at the same line in the input file

### Optimal trajectory determination

This program calculates the optimal trajectory for each zone, in order for the UAV to visit each point exactly once while minimizing the travelled distance. This problem exactly corresponds to the [Travelling salesman problem](https://en.wikipedia.org/wiki/Travelling_salesman_problem), which is known to be NP-complete and thus the exact solution is hard to compute.

Run the program:
```bash
path [points] [classes?] [HEURISTIC] (method:annealing/covering) (output=permutation.txt) (data=none)
```

Arguments:
- `points`: path to a `.csv` file containing following columns: `x,y,weight`
- `classes?`: path to a `.txt` file describing the zones, so the output of the [zones program](#zones)
    - Optional, set to `null` if not relevant
- `HEURISTIC`: heuristic value to start the annealing algorithm with
    - Only applied when method is annealing
- `method`: the chosen method to determine the trajectory
    - Options available: `annealing` (_Simulated annealing_ algorithm), ~~`covering` (_Covering tree_ algorithm)~~ Deprecated
- `output` (default: _permutation.txt_): path to the output file describing the permutation, organized as follow:
    - A line with two integers: the ID of the zone, and the number `n` of points in the zone
    - `n` lines with `x` and `y` coordinates of the points, ordered accordingly to the permutation
- `data` (default: _none_): path to the folder where to output data from the annealing process, i.e. the total distance of the trajectory over the iteration number

### Preview of the trajectory and spline fitting

Run the program:
```bash
preview [input] [XMIN] [XMAX] [YMIN] [YMAX] (method) (output=output.geojson)
```

Arguments:
- `input`: path to a text file describing a permutation, so the output of the [trajectory program](#optimal-trajectory-determination)
- `XMIN`, `XMAX`, `YMIN`, `YMAX`: coordinates for the boundaries of the output
    - Only used when method is `bezier` or `chart`
- `method`: 
    - Options available: `geojson` (Write a GeoJSON file), ~~`bezier` (Plot a bezier curve), `chart` (Plot a SVG chart)~~ Deprecated, use [Python scripts](./preview/plot-scripts/)
- `output` (default: _output.geojson_): path to the output file

## 📒 References

All geographical data used for this project comes from Institut Géographique National - Géoservices : https://geoservices.ign.fr/telechargement

Bibliography:

- John R. Gilbert, Gary L. Miller, Shang-Hua Teng : Geometric Mesh Partitioning: Implementation and Experiments, 1994, Xerox Palo Alto Research Center
- Simulated Annealing: The Travelling Salesman Problem : https://www.fourmilab.ch/documents/travelling/anneal/
- 2D and 3D Traveling Salesman Problem, Yil Haximusa, Zygmunt Pizlo, Laura Arns https://www.researchgate.net/profile/Yll-Haxhimusa/publication/48202356_2D_and_3D_Traveling_Salesman_Problem/links/0c96053355c132dabf000000/2D-and-3D-Traveling-Salesman-Problem.pdf?origin=publication_detail
- SFMU, SAMU, Ensevelis: https://sofia.medicalistes.fr/spip/IMG/pdf/Ensevelis.pdf
- Drone “humanitaire” : état de l’art et réflexions, Ludovic Apvrille, Tullio Tanzi, Yves Roudier et Jean-Luc Dugelay [https://rfpt.sfpt.fr/index.php/RFPT/article/view/20](https://rfpt.sfpt.fr/index.php/RFPT/article/view/201)1

You can get more details on the [Notion page](https://theo-vidal.notion.site/TIPE-2023-La-ville-e76f332f69aa47368b4cce6426bfec1d?pvs=74) of the project (in French).

## 🔐 License

    DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE 
            Version 2, December 2004 

Copyright (C) 2004 Sam Hocevar <sam@hocevar.net> 

Everyone is permitted to copy and distribute verbatim or modified 
copies of this license document, and changing it is allowed as long 
as the name is changed. 

        DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE 
TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION 

0. You just DO WHAT THE FUCK YOU WANT TO.
