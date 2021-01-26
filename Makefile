.PHONY: clean
clean:
	eldev clean all

.PHONY: prepare
prepare:
	eldev -C --unstable -p -dtT prepare

.PHONY: lint
lint:
	eldev -C --unstable -p -dtT lint

.PHONY: test
test:
	eldev -C --unstable -p -dtT test

docs:
	make -C doc all

html:
	make -C doc html-dir

install: install-docs

install-docs: docs
	make -C doc install-docs

install-info: info
	make -C doc install-info
