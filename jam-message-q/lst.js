
const fetch = require('node-fetch')

var data =  {}

fetch('http://localhost:8000/topic-list', {
    method: 'GET',
    headers: { 'Content-Type' : 'application/json' },
})

.then(res => res.text() )
.then(txt => console.log(txt));


