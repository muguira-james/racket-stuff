
web3-p: web3-p.rkt
	rm -rf build
	raco exe web3-p.rkt
	raco distribute build web3-p
	docker build -t web3 .

jamq: jamq.rkt
	rm -rf build
	raco  exe jamq.rkt
	raco distribute build  jamq
	docker build -t jamq1 .

clean:
	rm -rf build web3-p jamq

distclean:
	rm -rf build web3-p jamq node_modules package-lock.json 
