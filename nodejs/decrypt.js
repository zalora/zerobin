/* npm install sjcl */

var sjcl = require('sjcl')

var args = process.argv.slice(2)
  ,  pass = args[0]
  ,  cont = args[1]

var out = sjcl.decrypt(pass, cont)
console.log(out)

