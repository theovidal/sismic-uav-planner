let shapefile = require("shapefile")
const { exit } = require("process")
const fs = require("fs")

const filename = process.argv[2]
if (filename === undefined) {
  console.error("Il faut passer un nom de fichier en argument.")
  exit()
}

fs.readFile(filename, (err, raw) => {
  if (err) throw err
  let data = raw.toJSON()
  console.log(JSON.stringify(data))
})

/*
shapefile.open(filename, filename)
  .then(source => {
    let results = []
    source.read().then(function log(result) {
      if (result.done) {
        parse(results)
        return
      }
      results.push(result.value)
      return source.read().then(log)
    })
  })
  .catch(error => console.error(error.stack))


function parse(results) {
  console.log(JSON.stringify(results[210000]))
}
*/