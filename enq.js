const fetch = require('node-fetch')

var data = {
    quename: "james",
    payload: [ "cooking", "slacking", "hacking" ]
}

fetch('http://localhost:8000/enque', {
	method: 'post',
	body: JSON.stringify(data),
	headers: { 'Content-Type' : 'application/json' },
})
.then(res => res.json() )
.then(json => console.log(json));


