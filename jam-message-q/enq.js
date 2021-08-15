const fetch = require('node-fetch')
const commandlineargs = require('command-line-args')

const optionDefinitions = [
    { name: 'que_name', alias: 'q', type: String },
    { name: 'payload', alias: 'p', type: String }
    ]

const options = commandlineargs(optionDefinitions)
console.log("options->", options)

var deft_q = "james";
var deft_payload = [ "cooking", "slacking", "hacking" ];

if (Object.keys(options).length != 0) {
    deft_q = options.que_name
    deft_payload  = options.payload
} 
    
var data = {
    topicname: deft_q,
    payload: deft_payload
}
console.log("sending...", data)

fetch('http://localhost:8000/enque', {
    method: 'post',
    body: JSON.stringify(data),
    headers: { 'Content-Type' : 'application/json' },
})
    .then(res => res.json() )
    .then(json => console.log(json));


