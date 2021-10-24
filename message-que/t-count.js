const fetch = require('node-fetch')


fetch('http://localhost:8000/topic-count', {
	method: 'GET',
	headers: { 'Content-Type' : 'application/json' },
})
.then(res => res.json() )
.then(json => console.log(json));


