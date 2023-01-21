let shapefile = require("shapefile")
const cliProgress = require('cli-progress')
const { exit } = require("process")
const { parseFeature } = require("./precalculation")

const bar = new cliProgress.SingleBar({}, cliProgress.Presets.shades_classic)

const dbname = process.argv[2]
const shp = process.argv[3]
if (dbname === undefined || shp === undefined) {
  console.error("Erreur: veuillez passer tous les arguments : <shapefile> <sqlite>")
  exit()
}

const sqlite3 = require('sqlite3').verbose()
const db = new sqlite3.Database(dbname, (err) => {
  console.log(err)
})

bar.start(1000000, 0)

shapefile.open(shp, shp)
  .then(source => {
    let total = 0
    source.read().then(function parse(result) {
      if (result.done) {
        db.close()
        return
      }
      let data = result.value
      if (data.type == "Feature") {
        let feature = parseFeature(data)
        /*db.run("INSERT INTO bati(lon, lat, type, weight, distance) VALUES($lon, $lat, $type, $weight, $dist)", {
          $lon: feature.lon,
          $lat: feature.lat,
          $type: feature.type,
          $weight: feature.type,
          $dist: feature.type
        })*/
      }
      total += 1
      if (total % 1000 === 0) bar.update(total)
      return source.read().then(parse)
    })
  })
  .catch(error => console.error(error.stack))
