const fetch = require('node-fetch')
const commandlineargs = require('command-line-args')

const optionDefinitions = [
    { name: 'que_name', alias: 'q', type: String },
    ]

const options = commandlineargs(optionDefinitions)
console.log("options->", options)

var deft_q = "james";


if (Object.keys(options).length != 0) {
    deft_q = options.que_name

} 
    
var data = {
    quename: deft_q,

}
console.log("sending...", data)

fetch('http://localhost:8000/topic-data', {
    method: 'post',
    body: JSON.stringify(data),
    headers: { 'Content-Type' : 'application/json' },
})
    .then(res => res.json() )
    .then(js => console.log(js));


