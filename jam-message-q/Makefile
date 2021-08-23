

jamq: jamq.rkt package-lock.json
	rm -rf build
	raco  exe jamq.rkt
	raco distribute build  jamq
	docker build -t jamq1 .

package-lock.json: package.json
	npm i


clean:
	rm -rf build  jamq

distclean:
	rm -rf build  jamq node_modules package-lock.json *~
