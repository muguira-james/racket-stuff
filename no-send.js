const fetch = require('node-fetch')

var data = {
name: "james",
age: 59,
hobbies: [ "cooking", "slacking", "hacking" ]
}

fetch('http://localhost:8000/example-post', {
	method: 'post',
	body: JSON.stringify(data),
	headers: { 'Content-Type' : 'application/json' },
})
.then(res => res.json() )
.then(json => console.log(json));


