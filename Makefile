
web3-p: web3-p web3-p.rkt
	rm -rf build
	raco exe web3-p.rkt
	raco distribute build web3-p
	docker build -t web3 .

clean:
	rm -rf build web3-p
