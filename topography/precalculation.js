const exp = require("constants")

function getProperty(result, name, defaultValue) {
    if (result.properties[name] === undefined) return defaultValue
    else return result.properties[name]
  } 

function calculateBarycenter(coordinates) {
    let n = coordinates.length
    let avg = [0, 0]
    coordinates.forEach(v => {
        avg[0] += v[0]
        avg[1] += v[1]
    })  
    return [ avg[0]/n, avg[1]/n ]
}

function parseFeature(data) {
    let weight = getProperty(data, "HAUTEUR", 0)
    let [lon, lat] = calculateBarycenter(data.geometry.coordinates[0])
    return {
        weight,
        distance: Math.exp(weight) - 1,
        lon,
        lat,
        type: getProperty(data, "USAGE1", "undef")
    }
}

module.exports = { parseFeature }
